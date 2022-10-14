;;;; Advent of code 2021 solutions
;;;; Copyright (C) 2022 Benedikt Steger <b.steger@protonmail.ch>
;;;; 
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Affero General Public License along with this program. If not, see <http://www.gnu.org/licenses/>.

(in-package :adventofcode2021)

;;;; Parser

(defun segment2symbol (segment)
  "In:  #\a .. #\g
   Out: :a .. :g
   Reasoning: easier handling expected."
  (cond ((char= #\a segment) :a)
        ((char= #\b segment) :b)
        ((char= #\c segment) :c)
        ((char= #\d segment) :d)
        ((char= #\e segment) :e)
        ((char= #\f segment) :f)
        ((char= #\g segment) :g)))
(segment2symbol #\f)

(defun sort-signal-pattern (signal-pattern)
  "Sort the SIGNAL-PATTERN (list of keywords) alphabetically."
  (sort signal-pattern
        (lambda (x y) (string< (format nil "~A" x)
                               (format nil "~A" y)))))

(defrule 08-signal-pattern (+ (character-ranges (#\a #\g)))
  ;;#'sort: transposition is not important as the focus is on substitution, ordered lists are easier to debug.
  ;;#'deduce-one relies on sorted input.
  (:lambda (input)
    (sort-signal-pattern (mapcar #'segment2symbol input))))
(parse '08-signal-pattern "cdfbe")

(defrule 08-output-value 08-signal-pattern)

(defrule 08-line (and (+ (and 08-signal-pattern (? " ")))
                      ;;Will already be eaten, but nevertheless.
                      (? " ")
                      "| "
                      (+ (and 08-output-value (? " "))))
  (:lambda (input)
    (let ((signal-patterns (elt input 0))
          (output-values (elt input 3)))
      (list (mapcar #'first signal-patterns)
            (mapcar #'first output-values)))))
(parse '08-line "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")

(defrule 08-file (and (+ (and 08-line (? new)))
                      (? new))
  (:lambda (input)
    (let ((entries (elt input 0)))
      (mapcar #'first entries))))

(defmethod input ((day (eql 8)) source)
  "List of isolated puzzles, each consisting of a list of signal patterns and a list of output-values."
  (parse '08-file (read-file day source)))
(input 8 T)
(input 8 'toy-example)
(input 8 'full)


;;;; Solution

#|
0 is abc efg → 6 segments
1 is   c  f  → 2 segments
2 is a cde g → 5 segments
3 is a cd fg → 5 segments
4 is  bcd f  → 4 segments
5 is ab d fg → 5 segments
6 is ab defg → 6 segments
7 is a c  f  → 3 segments
8 is abcdefg → 7 segments
9 is abcd fg → 6 segments
→ GROUP BY segment (count) →
2 segments: 1
3 segments: 7
4 segments: 4
5 segments: 2 3 5
6 segments: 0 6 9
7 segments: 8

1 7 4 8 are determined by the length of input.
On to part 2.

Overall segment analysis:
a 0   2 3   5 6 7 8 9 → a has 8 candidates
b 0       4 5 6   8 9 → b has 6 candidates
c 0 1 2 3 4     7 8 9 → c has 8 candidates
d     2 3 4 5 6   8 9 → d has 7 candidates
e 0   2       6   8   → e has 4 candidates
f 0 1   3 4 5 6 7 8 9 → f has 9 candidates
g 0   2 3   5 6   8 9 → g has 7 candidates
Part1 for completeness:
    2 segments for 1
          4 segments for 4
                3 segments for 7
                  7 segments for 8

→ b e f can be identified through the histogram of the input.
|#

(defun frequency-analysis (signal-patterns)
  "Returns a property list which lists the segment counts."
  (let* ((population (loop for x in signal-patterns
                           append x))
         (tally (make-hash-table)))
    (loop for x in population
          do (setf (gethash x tally)
                   (incf (gethash x tally 0))))
    (loop for k being the hash-keys of tally
          for v being the hash-value of tally
          append (list k v))))
;;=> (:A 8 :B 9 :C 7 :D 8 :E 6 :F 7 :G 4)
(frequency-analysis (first (first (input 8 'toy-example))))

(defun fresh-key ()
  "Returns a fresh key. Values are cleartext, use (getf key crypted) to get the cleartext."
  '(:a nil :b nil :c nil :d nil :e nil :f nil :g nil))

(defun register-key-entry (crypted clear &key (key (fresh-key)))
  "Register an identified segment.
   KEY maps crypted to clear."
  (loop for (a . d) on key by #'cddr
        append (list a
                     (if (eql a crypted)
                         clear
                         (car d))))
  #| (setf (getf key crypted) clear)
  key|#)
;;=> (:A NIL :B NIL :C NIL :D NIL :E NIL :F NIL :G :E)
(register-key-entry :g :e)

(defun reverse-lookup (frequency-table count)
  "Returns the (first) key of a certain value in a property list, with naming fitted to the context of #'frequency-analysis."
  (loop for (a . d) on frequency-table by #'cddr
        when (= count (car d))
          do (return a)))
;;=> :E
(reverse-lookup (frequency-analysis (first (first (input 8 'toy-example)))) 6)

(defun deduce-bef (frequency-table &key (key (fresh-key)))
  "Deduce b e f segments:
   b is the only one which has 6 candidates.
   e is the only one which has 4 candidates.
   f is the only one which has 9 candidates."
  (register-key-entry
   (reverse-lookup frequency-table 6) :b
   :key (register-key-entry
         (reverse-lookup frequency-table 4) :e
         :key (register-key-entry
               (reverse-lookup frequency-table 9) :f
               :key key))))
;;=> (:A NIL :B :F :C NIL :D NIL :E :B :F NIL :G :E)
(deduce-bef (frequency-analysis (first (first (input 8 'toy-example)))))

#|
Next, partially decrypted numbers can be used. For example, the other entry of 1 must be c.
|#

(defun segments-of-number (signal-patterns number)
  "Return the (encrypted) segments belonging to a number.
   Sufficient information is only available for 1 4 7 8."
  (flet ((find-by-length (wished-length signal-patterns)
           "Pick the list with WISHED-LENGTH."
           (find-if (lambda (x) (= wished-length (length x))) signal-patterns)))
    (cond ((= 1 number) (find-by-length 2 signal-patterns))
          ((= 4 number) (find-by-length 4 signal-patterns))
          ((= 7 number) (find-by-length 3 signal-patterns))
          ((= 8 number) (find-by-length 7 signal-patterns))
          (T (error "Insufficient information.")))))
(segments-of-number (first (first (input 8 'toy-example))) 1)
(handler-case (segments-of-number (first (first (input 8 'toy-example))) 5)
  (error () "Failed as expected."))

(defun deduce-one (&key helper-segment helper-number signal-patterns (key (error "Call (at least) #'deduce-bef for a valid key.")))
  "Deduce a ciphertext→cleartext mapping for HELPER-SEGMENT with the help from HELPER-NUMBER.
   Needs a partial KEY recovered by (at least) #'deduce-bef.
   Only works for helpers which will know all but one ciphertext segment."
  (let* ((segments (segments-of-number signal-patterns helper-number))
         ;;Contract: only one nil in list.
         (decrypted-segments (mapcar (lambda (x) (getf key x)) segments))
         ;;This ciphertext segment has the same index as the nil.
         (encrypted-a (loop for i = 0 then (incf i)
                            for entry in decrypted-segments
                            unless entry
                              do (return (elt segments i)))))
    ;;It is known that HELPER-SEGMENT is the missing information.
    (register-key-entry encrypted-a helper-segment :key key)))
(deduce-one
 :helper-segment :c
 :helper-number 1
 :signal-patterns (first (first (input 8 'toy-example)))
 :key (deduce-bef (frequency-analysis (first (first (input 8 'toy-example))))))

#|
Now b c e f are known.
a d g are left.
Next helper is 7 (a c f) which will give the cleartext of a.
|#

;;=> (:A :C :B :F :C NIL :D :A :E :B :F NIL :G :E)
(deduce-one
 :helper-segment :a
 :helper-number 7
 :signal-patterns (first (elt (input 8 T) 0))
 :key (deduce-one
       :helper-segment :c
       :helper-number 1
       :signal-patterns (first (first (input 8 'toy-example)))
       :key (deduce-bef (frequency-analysis (first (first (input 8 'toy-example)))))))

#|
Now a b c e f are known.
d g are left.
Next helper is 4 (b c d f) which will give the cleartext of a.
|#

;;=> (:A :C :B :F :C NIL :D :A :E :B :F :D :G :E)
(deduce-one
 :helper-segment :d
 :helper-number 4
 :signal-patterns (first (first (input 8 'toy-example)))
 :key (deduce-one
       :helper-segment :a
       :helper-number 7
       :signal-patterns (first (first (input 8 'toy-example)))
       :key (deduce-one
             :helper-segment :c
             :helper-number 1
             :signal-patterns (first (first (input 8 'toy-example)))
             :key (deduce-bef (frequency-analysis (first (first (input 8 'toy-example))))))))

#|
Now a b c d e f are known.
g is left.
Next helper is 8 (a b c d e f g) which will give the cleartext of g.
|#

(defun deduce-segments (signal-patterns)
  "The key needed to decrypt the ciphertext segments."
  (deduce-one
   :helper-segment :g
   :helper-number 8
   :signal-patterns signal-patterns
   :key (deduce-one
         :helper-segment :d
         :helper-number 4
         :signal-patterns signal-patterns
         :key (deduce-one
               :helper-segment :a
               :helper-number 7
               :signal-patterns signal-patterns
               :key (deduce-one
                     :helper-segment :c
                     :helper-number 1
                     :signal-patterns signal-patterns
                     :key (deduce-bef (frequency-analysis signal-patterns)))))))
;;=> (:A :C :B :F :C :G :D :A :E :B :F :D :G :E)
(deduce-segments (first (first (input 8 'toy-example))))

(defun decrypt-signal-pattern (signal-pattern key)
  "Return the cleartext segments of a SIGNAL-PATTERN with the help of KEY."
  (sort-signal-pattern (mapcar (lambda (x) (getf key x)) signal-pattern)))
;;Encrypted: 7 is (:A :B :D).
(first (elt (input 8 'toy-example) 0))
;;Decrypted: 7 is (:A :C :F) (correct).
(let ((key (deduce-segments (first (first (input 8 'toy-example))))))
  (mapcar (lambda (x) (decrypt-signal-pattern x key))
          (first (first (input 8 'toy-example)))))

(defun number-lookup (cleartext-signal-pattern)
  "Return the number which will need all segments listed in CLEARTEXT-SIGNAL-PATTERN."
  (flet ((pattern-match? (pattern)
           (and
            ;;Special case '(:a) would determine the list length for #'every.
            (= (length pattern) (length cleartext-signal-pattern))
            (every #'eql
                   ;;Better be safe than sorry.
                   (sort-signal-pattern cleartext-signal-pattern)
                   pattern))))
    (cond ((pattern-match? '(:a :b :c :e :f :g)) 0)
          ((pattern-match? '(:c :f)) 1)
          ((pattern-match? '(:a :c :d :e :g)) 2)
          ((pattern-match? '(:a :c :d :f :g)) 3)
          ((pattern-match? '(:b :c :d :f)) 4)
          ((pattern-match? '(:a :b :d :f :g)) 5)
          ((pattern-match? '(:a :b :d :e :f :g)) 6)
          ((pattern-match? '(:a :c :f)) 7)
          ((pattern-match? '(:a :b :c :d :e :f :g)) 8)
          ((pattern-match? '(:a :b :c :d :f :g)) 9)
          (T nil))))
(number-lookup '(:a))

(defun output-value-of-entry (input-line)
  "Returns the number an INPUT-LINE represents.
   Format of INPUT-LINE: see parser rule '08-line ."
  (let ((key (deduce-segments (first input-line))))
    (parse-integer
     (format nil "~{~A~}"
             (loop for x in (second input-line)
                   collect (number-lookup
                            (decrypt-signal-pattern
                             (sort-signal-pattern x)
                             key)))))))
;;5353
(output-value-of-entry (first (input 8 'toy-example)))
(loop for line in (input 8 T)
      collect (output-value-of-entry line))

(defun digit-1-4-7-8? (signal-pattern)
  "Returns T if SIGNAL-PATTERN is 1 or 4 or 7 or 8.
   SIGNAL-PATTERN may be a ciphertext or a cleartext - does not matter."
  (let ((length (length signal-pattern)))
    (or
     ;;number 1
     (= length 2)
     ;;number 4
     (= length 4)
     ;;number 7
     (= length 3)
     ;;number 8
     (= length 7))))

(defmethod solution ((day (eql 8)) (variant (eql 'part1)) source)
  "Sum all occurences of numbers 1 or 4 or 7 or 8"
  (loop for line in (input day source)
        summing (loop for signal-pattern in (second line)
                      summing (if (digit-1-4-7-8? signal-pattern)
                                  1
                                  0))))
(is 8 'part1 26)

(defmethod solution ((day (eql 8)) (variant (eql 'part2)) source)
  "Sum all output values of all entries."
  (loop for line in (input day source)
        summing (output-value-of-entry line)))
(is 8 'part2 61229)

