;;;; Advent of code 2021 solutions
;;;; Copyright (C) 2022 Benedikt Steger <b.steger@protonmail.ch>
;;;; 
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Affero General Public License along with this program. If not, see <http://www.gnu.org/licenses/>.

(in-package :adventofcode2021)

#|
A migration of an old software / program / code base without any call to my DSL library lisp-at-work.
|#

;;;; Parser

;;; This parser of aoc21-alu intentionally makes convenience shortcuts.

(defrule alu-symbol (or "w" "x" "y" "z")
  (:lambda (input)
    (cond ((string= "w" input) 'w)
          ((string= "x" input) 'x)
          ((string= "y" input) 'y)
          ((string= "z" input) 'z))))
(parse 'alu-symbol "w")

(defrule alu-atom (or integer-z alu-symbol))
(parse 'alu-atom "-123")
(parse 'alu-atom "w")

(defrule alu-inp (and "inp " alu-symbol)
  (:lambda (input)
    (list :this-is 'alu-inp :place (elt input 1))))
(parse 'alu-inp "inp w")

(defrule alu-add (and "add " alu-symbol " " alu-atom)
  (:lambda (input)
    (let ((place (elt input 1))
          (value (elt input 3)))
      (list :this-is 'alu-add :place place :value value))))
(parse 'alu-add "add w -1")

(defrule alu-mul (and "mul " alu-symbol " " alu-atom)
  (:lambda (input)
    (let ((place (elt input 1))
          (value (elt input 3)))
      (list :this-is 'alu-mul :place place :value value))))
(parse 'alu-mul "mul x -1")

(defrule alu-div (and "div " alu-symbol " " alu-atom)
  (:lambda (input)
    (let ((place (elt input 1))
          (value (elt input 3)))
      (list :this-is 'alu-div :place place :value value))))
(parse 'alu-div "div x -1")

(defrule alu-mod (and "mod " alu-symbol " " alu-atom)
  (:lambda (input)
    (let ((place (elt input 1))
          (value (elt input 3)))
      (list :this-is 'alu-mod :place place :value value))))
(parse 'alu-mod "mod x 3")

(defrule alu-eql (and "eql " alu-symbol " " alu-atom)
  (:lambda (input)
    (let ((place (elt input 1))
          (value (elt input 3)))
      (list :this-is 'alu-eql :place place :value value))))
(parse 'alu-eql "eql x 3")

(defrule 24-file (+ (and (or alu-inp alu-add alu-mul alu-div alu-mod alu-eql) (? new)))
  (:lambda (input)
    (mapcar #'first input)))
(parse '24-file "inp x
mul x -1
")

(defmethod input ((day (eql 24)) source)
  "AST (or merely an abstract syntax list) with aoc21-alu primitives."
  (parse '24-file (read-file 24 source)))
;;=> ((:this-is alu-inp :place x) (:this-is alu-mul :place x :value -1))
(input 24 'toy-example)
(input 24 'middle-example)
(input 24 T)
(input 24 'full)


;;;; Solution

#|
Determine the problem size.
|#

(loop for i from 0 below 36
      collect (format nil "~9,14,'0R" i))
;;0..22876792454960 → OK, ad fontes...
(identity #9R88888888888888)
(length (format nil "~9,14R" 22876792454960))
(length (format nil "~9,14R" (1+ 22876792454960)))

#|
aoc21-alu's operations are supported by Common Lisp, too.
Lispify aoc21-alu.
|#

(defgeneric lispify-aoc21-alu (op place &optional value)
  (:documentation "Represent the aoc21-alu-operations as Common Lisp forms. Needs additional setup context (registers 'w 'x 'y 'z, which are initialized to 0, and 'arguments).")
  (:method ((op (eql 'alu-inp)) place &optional value)
    (declare (ignore value))
    ;;`(setf ,place (pop arguments))
    `(progn (setf ,place (aref arguments position))
            (incf position)))
  (:method ((op (eql 'alu-add)) place &optional value)
    `(incf ,place ,value))
  (:method ((op (eql 'alu-mul)) place &optional value)
    `(setf ,place (* ,place ,value)))
  (:method ((op (eql 'alu-div)) place &optional value)
    `(setf ,place (truncate ,place ,value)))
  (:method ((op (eql 'alu-mod)) place &optional value)
    `(setf ,place (rem ,place ,value)))
  (:method ((op (eql 'alu-eql)) place &optional value)
    `(setf ,place (if (= ,place ,value) 1 0))))

(defun lispified-program (input-ast &key compile-form?)
  "Lispify the aoc21-alu program. Return a callable compiled function if COMPILE-FORM?, the plain form otherwise."
  (let ((program `(lambda (arguments)
                    (let ((w 0)
                          (x 0)
                          (y 0)
                          (z 0)
                          (position 0))
                      ,@(loop for operation in input-ast
                              collect (lispify-aoc21-alu
                                       (getf operation :this-is)
                                       (getf operation :place)
                                       ;;NIL for alu-inp, which is ok.
                                       (getf operation :value)))
                      (list w x y z)))))
    (if compile-form?
        (compile nil program)
        program)))
(lispified-program (input 24 'toy-example))
;;Target: (0 -1 0 0).
(funcall (lispified-program (input 24 'toy-example) :compile-form? T)
         (make-array (list 1) :element-type 'fixnum :initial-contents '(1)))
;;Target: (0 6 0 1).
(funcall (lispified-program (input 24 'middle-example) :compile-form? T)
         (make-array (list 2) :element-type 'fixnum :initial-contents '(2 6)))
;;Target: (0 7 0 0).
(funcall (lispified-program (input 24 'middle-example) :compile-form? T)
         (make-array (list 2) :element-type 'fixnum :initial-contents '(2 7)))
;;Target: ((0 0 0 1) (0 0 1 0) (0 0 1 1) (0 1 0 0) (0 1 0 1) (0 1 1 0) (0 1 1 1) (1 0 0 0) (1 0 0 1) (1 0 1 0) (1 0 1 1) (1 1 0 0) (1 1 0 1) (1 1 1 0) (1 1 1 1)).
(let ((fun (lispified-program (input 24 'example) :compile-form? T)))
  (loop for i from 0 below 16
        collect (funcall fun (make-array (list 1) :element-type 'fixnum :initial-contents (list i)))))

#|(with-open-file (f "/dev/shm/aoc" :direction :output :if-does-not-exist :create :if-exists :supersede)
  (format f "~A" (lispified-program (input 24 'full))))|#

#|
Analysis of the program code shows a repeating pattern.
The puzzle input does something similar digit after digit.
 This digit-weaver is an inlined function.
Verify those assumptions.
|#

(defun de-expand-list-pattern (input)
  "Assuming the pattern ,@(loop for i from 0 below 14 collect `(weave-digit i)), return list of `(weave-digit i)."
  (cond
    ;;18: manually determined/inferred from INPUT.
    ((<= 18 (length input))
     (cons (subseq input 0 18) (de-expand-list-pattern (subseq input 18))))
    (input (error "Expanded program code seemingly does not contain a repeated list of 18 operations."))))
(de-expand-list-pattern (input 24 'full))
(handler-case (de-expand-list-pattern (input 24 'toy-example))
  (error () "Failed as expected."))

(defun identify-inlined-funcall (&key expansions ast-so-far arguments (argument-placeholders '(a b c d e f g h i j k l m)))
  "Grow the list of shared operations (AST-SO-FAR) until items of EXPANSIONS differ laterally.
   Flag the operation, add a list of all possible inputs to ARGUMENTS, and continue until the whole operation list is covered.
   Returns a function/macro body stub as the first return value and the list of arguments for each expansion as the second return value."
  (if (>= (length ast-so-far) (length (first expansions)))
      (values (reverse ast-so-far)
              (when arguments
                ;;Transpose the ARGUMENTS matrix in order to get the parameters of each expansion.
                (apply #'mapcar #'list (reverse arguments))))
      (let ((operations-in-question
              (loop for expansion in expansions
                    collect (elt expansion (if ast-so-far (length ast-so-far) 0)))))
        (if (= 1 (length (remove-duplicates operations-in-question :test #'equal)))
            ;;No variation across expansions.
            (identify-inlined-funcall
             :expansions expansions
             :ast-so-far (cons (first operations-in-question) ast-so-far)
             :arguments arguments
             :argument-placeholders argument-placeholders)
            ;;Variation detected. Flag it and add the variations to ARGUMENTS.
            ;;Not hunting down the difference within the operation.
            ;; Input is small enough for the educated guess that it is solely the value which differs.
            ;; A recursive tree walk would cover this difference on a "real" AST anyway.
            (identify-inlined-funcall
             :expansions expansions
             :ast-so-far (cons
                          (let ((ori-op (first operations-in-question)))
                            (list :this-is (getf ori-op :this-is)
                                  :place (getf ori-op :place)
                                  :value (or
                                          ;;Contract: no other variable name than w x y z.
                                          (car argument-placeholders)
                                          (gensym "PROBABLY-NOT-SHARING-SIMILARITIES"))))
                          ast-so-far)
             :arguments (cons (loop for op in operations-in-question
                                    collect (getf op :value))
                              arguments)
             :argument-placeholders (rest argument-placeholders))))))
;;Arguments full: ((1 11 7) (1 14 8) (1 10 16) (1 14 8) (26 -8 3) (1 14 12) (26 -11 1) (1 10 8) (26 -6 8) (26 -9 14) (1 12 4) (26 -5 14) (26 -4 15) (26 -9 6))
(multiple-value-list (identify-inlined-funcall :expansions (de-expand-list-pattern (input 24 'full))))

#|
Check whether the patterns were correctly abstracted: compare the expansion with the expanded input.
|#

;;T
(equal (input 24 'full)
       (multiple-value-bind (ast arguments)
           (identify-inlined-funcall :expansions (de-expand-list-pattern (input 24 'full)))
         (loop for (a b c) in arguments
               append
               ;;Unquote a b c manually.
               (loop for op in ast
                     collect (substitute
                              a 'a (substitute
                                    b 'b (substitute c 'c op)))))))

#|
Now that the consistent use of the digit weaver is verified, it is time to analyze it.
I dissect the digit weaver by reimplementing it.
|#

#|Examining the isolation degree of the inlined function:
w is used as an argument variable. No writes to w happen after the input operation.
x is set to 0 at the beginning of the function.
y is not affected by the assignments that occur before y is set to 0.
z is not resetted.
→ Lexical binding for w x y, indefinite scope for z.
→ z and w are arguments, and x and y are established by the special operator "let".

It would make sense to write automated verifiers for those new assumptions if the input were larger. Not this time.|#
(format nil "~%~S~%" (mapcar (lambda (x)
                               (lispify-aoc21-alu
                                (getf x :this-is)
                                (getf x :place)
                                (getf x :value)))
                           (identify-inlined-funcall :expansions (de-expand-list-pattern (input 24 'full)))))

;;↓↓↓↓ Copying the printed form to a new function. ↓↓↓↓

(defun weave-digit-imperative (a b c w &optional (z 0))
  "A function that is to be reverse engineered and whose sole purpose of existence is puzzling fun."
  (let ((x 0)
        (y 0))
    ;;See the lambda list of this defun.
    ;;(setf w w)
    
    ;;(let ((x 0)))
    (setf x (* x 0))
    ;;(let ((x z)))
    (incf x z)
    ;;(let ((x (rem z 26))))
    (setf x (rem x 26))
    #+only-because-of-C-c-M-q-identation
    (let ((x (rem z 26))
          (z (truncate z a))))
    (setf z (truncate z a))
    #+only-because-of-C-c-M-q-identation
    (let ((x (+ (rem z 26) b))
          (z (truncate z a))))
    (incf x b)
    #+only-because-of-C-c-M-q-identation
    (let ((x (if (= (+ (rem z 26) b) w)
                 1
                 0))
          (z (truncate z a))))
    (setf x (if (= x w)
                1
                0))
    #+only-because-of-C-c-M-q-identation
    (let ((x (if (= (+ (rem z 26) b) w)
                 0
                 1))
          (z (truncate z a))))
    (setf x (if (= x 0)
                1
                0))
    
    ;;Next 5 operations: (let ((z (* z (1+ (* 25 x))))))
    
    #+only-because-of-C-c-M-q-identation
    (let ((x (if (= (+ (rem z 26) b) w)
                 0
                 1))
          (z (* (truncate z a) (1+ (* 25 x))))))
    (setf y (* y 0))
    (incf y 25)
    (setf y (* y x))
    (incf y 1)
    (setf z (* z y))
    
    ;;Next 5 operations: (let ((z (+ z (* (+ w c) x)))))
    
    #+only-because-of-C-c-M-q-identation
    (let ((x (if (= (+ (rem z 26) b) w)
                 0
                 1)))
      (let ((z (+ (* (truncate z a) (1+ (* 25 x)))
                  (* (+ w c) x))))))
    (setf y (* y 0))
    (incf y w)
    (incf y c)
    (setf y (* y x))
    (incf z y)
    
    ;;(list w x y z)
    z))

(defun weave-digit (a b c w &optional (z 0))
  "#'weave-digit-imperative in functional style."
  #+only-because-of-C-c-M-q-identation
  (let ((x (if (= (+ (rem z 26) b) w)
               0
               1)))
    (+ (* (truncate z a)
          (1+ (* 25 x)))
       (* (+ w c)
          x)))
  (if (= (+ (rem z 26) b) w)
      (truncate z a)
      (+ (* (truncate z a) 26) w c)))

(defun weave-digits (&key abc remaining-digits (z 0) (weaving-function #'weave-digit))
  "Execute the program of puzzle 24.
   The keyword arguments are useful for a stepwise analysis since any point in execution can be adopted at will."
  (if remaining-digits
      (weave-digits
       :abc (rest abc)
       :remaining-digits (rest remaining-digits)
       :z (funcall weaving-function
                   (first (first abc))
                   (second (first abc))
                   (third (first abc))
                   (car remaining-digits)
                   z)
       :weaving-function weaving-function)
      z))

;;Gain some level of confidence in the logical equivalence between the original, #'weave-digit-imperative and #'weave-digit.
;;T
(let* ((input (input 24 'full))
       (compiled-function (lispified-program input :compile-form? T)))
  (multiple-value-bind (ast arguments)
      (identify-inlined-funcall :expansions (de-expand-list-pattern input))
    (declare (ignore ast))
    ;;100: this form is executed during #'ql:quickload.
    (loop for i from 0 below 100
          always (let ((numbers (loop for i from 0 below 14 collect (1+ (random 9)))))
                   (= (fourth (funcall compiled-function
                                       (make-array (list 14) :element-type 'fixnum :initial-contents numbers)))
                      (weave-digits :abc arguments :remaining-digits numbers :weaving-function #'weave-digit-imperative)
                      (weave-digits :abc arguments :remaining-digits numbers :weaving-function #'weave-digit))))))

(defmethod real-input ((day (eql 24)) source)
  "The varying parts of the inlined digit-weaving function (#'weave-digit) in the puzzle input.
   Returns a list of a/b/c argument values used per invocation of the #'weave-digit function."
  (multiple-value-bind (ast arguments)
      (identify-inlined-funcall :expansions (de-expand-list-pattern (input 24 source)))
    (declare (ignore ast))
    arguments))
(real-input 24 'full)
;;Easier to read.
(apply #'mapcar #'list (real-input 24 'full))

#|
Solve for max/min w (z=0 actually) with the help of a b c.
Try to reduce the number/scope of variables and try to find patterns/invariants.
|#

;;Can't solve for w in isolation: z has indefinite scope (a b c w → z).
;;NIL
(loop for abc in (real-input 24 'full)
      always (remove-if-not #'zerop
                             (loop for i from 1 below 10
                                   collect (weave-digit (first abc) (second abc) (third abc) i))))

;;But `a is superfluous (b → a, b c w → z):
;;T
(loop for abc in (real-input 24 'full)
      always (= (first abc)
                (if (plusp (second abc)) 1 26)))

;;Assumption: b begins with an ascending step and finally returns to 0.
;;T
(let ((b-list (mapcar (lambda (x) (signum (second x))) (real-input 24 'full))))
  (and (= 1 (car b-list))
       (= -1 (car (last b-list)))
       (= 0 (apply #'+ b-list))))

#|
Continuing this line of thinking, b must always be larger than 1..9 if it's positive (well, it is). Why? Because b controls the execution path this way (remainder of z is 0..25 in IF's TEST). It must deterministically control it since the contract says that 0 has to be reached at the end and since #'weave-digit is used repeatedly in the same manner.

(if
 ;;b steers away from z=0 or towards z=0.
 (= (+ (rem z 26) b) w)
 ;;b steers towards z=0 (it is negative here). The real form is (truncate z 26).
 (truncate z (if (plusp b) 1 26))
 ;;b steers away from z=0. Since b is positive, the real form is (+ (* z 26) w c).
 (+ (* (truncate z (if (plusp b) 1 26)) 26) w c))

Since b must be able to steer stepwise towards or away from z=0, (+ w c) in IF's ELSE must be lower than 26 (and they are). Why? Because w and c should not interfere with the THEN part of the IF. ... Aha! ... The modulo operation is used as a way to transport information - it packs a stack in a single integer. The forms with b in the THEN and ELSE part of the special operator "if" cancel each other out. The problem is now reduced to the two crucial forms (= (+ (rem z 26) b) w) and (+ w c).

Additional invariants can be found now. c is only relevant for w if b is positive. The value of b is only relevant for w if b is negative.

Each step away from z=0 must be matched with a step towards z=0. Now that it is known that a stack is used, the indefinite extent of z is finally reduced to dynamic extent (this is the "scope in time" in Common Lisp's parlance). This means that z will be the same before and after a push and pop of the same level. For the outermost level, this means z=0 at the beginning and z=0 at the end (or any number wanted).

The next thing that needs to be tackled is, consequently, the retrieval of push-pop-teams. Let's call the two positions that belong to the same nesting level of stack operations "team members".
|#

(defun team-member-distance (remaining-bs &key height (counter 0))
  "The number of steps towards the other team member.
   REMAINING-BS should be normalized to 1 or -1 values, respectively."
  (if (and height (= 0 height))
      (1- counter)
      (team-member-distance
       (rest remaining-bs)
       :height (+ (or height 0) (car remaining-bs))
       :counter (1+ counter))))

(defun team-member-lut (real-input)
  "A list which tells at each index at what other index the matching opposite-steering team member is."
  (let ((b-list (mapcar (lambda (x) (signum (second x))) real-input)))
    (loop with i = 0
          for b on b-list
          collect (if (plusp (car b))
                      (+ i (team-member-distance b))
                      ;;The reverse needs the element which is exactly one out of reach of ":for b-reverse :on (reverse b-list)".
                      (- i (team-member-distance (reverse (subseq b-list 0 (1+ i))))))
          do (incf i))))
(team-member-lut (real-input 24 'full))

#|
Find out how the arguments determine the w of the left team member and the w of the right team member.
|#

;;1. Identify the pairs.
(team-member-lut (real-input 24 'full))
;;2. Go to a pair by filling in nines. Memorize the z value.
(weave-digits :abc (real-input 24 'full) :remaining-digits (list 9 9))
;;3. Identify a correct answer. The correct answer is one which results in the same z value as above.
(loop for m from 1 below 10
      append (loop for n from 1 below 10
                   when (= (weave-digits :abc (real-input 24 'full) :remaining-digits (list 9 9))
                           (weave-digits :abc (real-input 24 'full) :remaining-digits (list 9 9 m n)))
                     collect (list m n)))
;;4. Fetch the arguments of the left team member and of the right team member.
(subseq (real-input 24 'full) 2 4)
;;5. Repeat for other pairs until at least three combinations are gathered. (By changing above forms.)
;;6. Manually search for the formula whilst keeping #'weave-digit in mind.

(defun biggest-for-team (left-c right-b)
  "What interplay of LEFT-C, RIGHT-B, left-w and right-w results in the same z after a push-pop-operation pair.
   Returns left-w and right-w.
   Maximize the first number.
   The rules of interplay are determined by #'weave-digit and #'real-input."
  (let* ((left-w (min 9 (- (+ 9 (* -1 right-b)) left-c)))
         (right-w (+ left-w left-c right-b)))
    (list left-w right-w)))

(defun smallest-for-team (left-c right-b)
  "Variant of #'biggest-for-team which reduces left-w.
   (right-w is irrelevant since left-w → right-w.)"
  (let* ((left-w (max 1 (- (+ 1 (* -1 right-b)) left-c)))
         (right-w (+ left-w left-c right-b)))
    (list left-w right-w)))

(defun fill-in-pair (real-input positioned-teams &key part2?)
  "Search for teams that are not positioned yet, calculate their numbers and recursively continue until all teams are positioned.
   Destructively modifies POSITIONED-TEAMS."
  (let ((next-free-index (position nil positioned-teams)))
    (if next-free-index
        (let* ((right-team-member-index
                 (elt (team-member-lut real-input) next-free-index))
               (left-abc
                 (elt real-input next-free-index))
               (right-abc
                 (elt real-input right-team-member-index))
               (digits
                 (funcall (if part2? #'smallest-for-team #'biggest-for-team)
                          (third left-abc)
                          (second right-abc))))
          (setf (elt positioned-teams next-free-index) (first digits)
                (elt positioned-teams right-team-member-index) (second digits))
          (fill-in-pair real-input positioned-teams :part2? part2?))
        positioned-teams)))
(fill-in-pair (real-input 24 'full) (loop repeat 14 collect nil))
(fill-in-pair (real-input 24 'full) (loop repeat 14 collect nil) :part2? T)

(defun solution-24 (day source &optional part2?)
  "Prepare the call to #'fill-in-pair for #'solution."
  (parse-integer
   (format nil "~{~A~}"
           (fill-in-pair (real-input day source)
                         (loop repeat 14 collect nil)
                         :part2? part2?))))

(defmethod solution ((day (eql 24)) (variant (eql 'part1)) source)
  "The largest MONAD number accepted by the submarine's MONAD number checker."
  (solution-24 day source))
;;No example for the part1 task.

(defmethod solution ((day (eql 24)) (variant (eql 'part2)) source)
  "The smallest MONAD number accepted by the submarine's MONAD number checker."
  (solution-24 day source T))
;;No example for the part2 task.


;;;; Appendix

#|
If the assumptions about the puzzle input do not hold true, brute force is needed.
Apart from the 14 inp instructions, the spec has no contract regarding input program structure after all.
|#

(defun prepare-number (number)
  "Represent NUMBER in an array for a #'lispified-program. Destructively modifies NUMBER."
  (make-array (list 14)
              :element-type 'fixnum
              :initial-contents (reverse (loop while (plusp number)
                                               collect (1+ (rem number 9))
                                               do (setf number (floor number 9))))))
(prepare-number #9R11111147238447)

(let ((compiled-monad-checker (lispified-program (input 24 'full) :compile-form? T)))
  ;;...08: disabled.
  (loop for i downfrom #9R88888888888888 downto #9R88888888888808
        when (= 0 (fourth (funcall compiled-monad-checker (prepare-number i))))
          return i))

#|
The completely migrated function.
|#

(defun 24-push (number stack)
  "Pushes NUMBER onto STACK (non-destructively).
   NUMBER is limited to the interval [0,26)."
  (when (<= 26 number)
    (error "NUMBER is limited to the interval [0,26)."))
  (+ (* 26 stack) number))
(24-push 25 1234)

(defun 24-pop (stack)
  "Retrieves the last number which was pushed onto STACK, and returns, as the second return value, the new stack.
   NIL if the stack is empty."
  (when (< 0 stack)
    (values (rem stack 26) (floor stack 26))))
(24-pop 32109)
(24-pop 0)

#|
Just for fun.
|#

(defun some-valid-monad-numbers (real-input)
  "By the virtue of the gathered insights, produce valid serial numbers for MONAD submarines.
   Exploits the fact that the team members can be randomized to a certain degree, since they result in the same z value.
   For example, each team can randomly select the part1 or the part2 variant."
  ;;I decided against the implementation of the required algorithm (which is straightforward in this situation).
  (declare (ignore real-input)))

;;Own recursive interpreter.
;;916660/5030 processor cycles → 182.2 times less efficient than #'lispified-program.
;;Also very unmaintainable.
(defun manually-interpret-aoc24-alu (&key remaining-operations remaining-arguments (w 0) (x 0) (y 0) (z 0))
  (flet ((my-eval (operation w x y z)
           (if (symbolp (getf operation :value))
               (case (getf operation :value)
                 (w w) (x x) (y y) (z z))
               (getf operation :value))))
    ;;THINK: program pointer?
    (if remaining-operations
        (let ((next (car remaining-operations)))
          (cond
            ((eql 'alu-inp (getf next :this-is))
             (manually-interpret-aoc24-alu
              :remaining-operations (rest remaining-operations)
              :remaining-arguments (rest remaining-arguments)
              :w (if (eql 'w (getf next :place)) (car remaining-arguments) w)
              :x (if (eql 'x (getf next :place)) (car remaining-arguments) x)
              :y (if (eql 'y (getf next :place)) (car remaining-arguments) y)
              :z (if (eql 'z (getf next :place)) (car remaining-arguments) z)))
            ((eql 'alu-add (getf next :this-is))
             (manually-interpret-aoc24-alu
              :remaining-operations (rest remaining-operations)
              :remaining-arguments remaining-arguments
              :w (if (eql 'w (getf next :place)) (+ w (my-eval next w x y z)) w)
              :x (if (eql 'x (getf next :place)) (+ x (my-eval next w x y z)) x)
              :y (if (eql 'y (getf next :place)) (+ y (my-eval next w x y z)) y)
              :z (if (eql 'z (getf next :place)) (+ z (my-eval next w x y z)) z)))
            ((eql 'alu-mul (getf next :this-is))
             (manually-interpret-aoc24-alu
              :remaining-operations (rest remaining-operations)
              :remaining-arguments remaining-arguments
              :w (if (eql 'w (getf next :place)) (* w (my-eval next w x y z)) w)
              :x (if (eql 'x (getf next :place)) (* x (my-eval next w x y z)) x)
              :y (if (eql 'y (getf next :place)) (* y (my-eval next w x y z)) y)
              :z (if (eql 'z (getf next :place)) (* z (my-eval next w x y z)) z)))
            ((eql 'alu-div (getf next :this-is))
             (manually-interpret-aoc24-alu
              :remaining-operations (rest remaining-operations)
              :remaining-arguments remaining-arguments
              :w (if (eql 'w (getf next :place)) (truncate w (my-eval next w x y z)) w)
              :x (if (eql 'x (getf next :place)) (truncate x (my-eval next w x y z)) x)
              :y (if (eql 'y (getf next :place)) (truncate y (my-eval next w x y z)) y)
              :z (if (eql 'z (getf next :place)) (truncate z (my-eval next w x y z)) z)))
            ((eql 'alu-mod (getf next :this-is))
             (manually-interpret-aoc24-alu
              :remaining-operations (rest remaining-operations)
              :remaining-arguments remaining-arguments
              :w (if (eql 'w (getf next :place)) (rem w (my-eval next w x y z)) w)
              :x (if (eql 'x (getf next :place)) (rem x (my-eval next w x y z)) x)
              :y (if (eql 'y (getf next :place)) (rem y (my-eval next w x y z)) y)
              :z (if (eql 'z (getf next :place)) (rem z (my-eval next w x y z)) z)))
            ((eql 'alu-eql (getf next :this-is))
             (manually-interpret-aoc24-alu
              :remaining-operations (rest remaining-operations)
              :remaining-arguments remaining-arguments
              :w (if (eql 'w (getf next :place)) (if (= w (my-eval next w x y z)) 1 0) w)
              :x (if (eql 'x (getf next :place)) (if (= x (my-eval next w x y z)) 1 0) x)
              :y (if (eql 'y (getf next :place)) (if (= y (my-eval next w x y z)) 1 0) y)
              :z (if (eql 'z (getf next :place)) (if (= z (my-eval next w x y z)) 1 0) z)))
            (T (error "What is ~A?" (getf next :this-is)))))
        (list w x y z))))
(manually-interpret-aoc24-alu
 :remaining-operations (input 24 'toy-example)
 :remaining-arguments '(1))
(manually-interpret-aoc24-alu
 :remaining-operations (input 24 'middle-example)
 :remaining-arguments '(2 6))
(manually-interpret-aoc24-alu
 :remaining-operations (input 24 'middle-example)
 :remaining-arguments '(2 7))
(loop for i from 0 below 16
      collect (manually-interpret-aoc24-alu
               :remaining-operations (input 24 'example)
               :remaining-arguments (list i)))

