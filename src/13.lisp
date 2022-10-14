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

(defrule 13-dot (and integer "," integer)
  (:lambda (input)
    ;;List with x and y.
    (list (elt input 0) (elt input 2))))
(parse '13-dot "6,10")

(defrule 13-folding-instruction (and "fold along " (or "x" "y") "=" integer)
  (:lambda (input)
    ;;List with (or 'x 'y) and axis.
    (list (read-from-string (elt input 1)) (elt input 3))))
(parse '13-folding-instruction "fold along y=7")

(defrule 13-file (and (+ (and 13-dot (? new)))
                      (? new)
                      (+ (and 13-folding-instruction (? new))))
  (:lambda (input)
    ;;List with dots and folding instructions.
    (list (mapcar #'first (elt input 0))
          (mapcar #'first (elt input 2)))))
(parse '13-file "6,10
0,14

fold along x=5")

(defmethod input ((day (eql 13)) source)
  "A sheet of paper with dots (first element in list) and folding instructions (second element in list) on it."
  (parse '13-file (read-file 13 source)))
(input 13 T)


;;;; Solution

(defun fold-general (sheet remaining-instructions)
  "Fold a SHEET according to REMAINING-INSTRUCTIONS."
  (if remaining-instructions
      (let ((axis
              ;;'x (vertical fold) or 'y (horizontal fold).
              (first (car remaining-instructions)))
            ;;Redundant information, as it is in the middle anyway.
            ;;But still useful for calculations.
            (axis-offset (second (car remaining-instructions))))
        (fold-general (remove-duplicates
                       (mapcar
                        (lambda (x)
                          (list
                           (if (and (eql 'x axis) (< axis-offset (first x)))
                               ;;Folding vertically = reduce x value bigger than folding line by the double distance to the folding line.
                               (- (first x) (* 2 (- (first x) axis-offset)))
                               (first x))
                           (if (and (eql 'y axis) (< axis-offset (second x)))
                               ;;Folding horizontally = reduce y value bigger than folding line by the double distance to the folding line.
                               (- (second x) (* 2 (- (second x) axis-offset)))
                               (second x))))
                        sheet)
                       :test #'equal)
                      (rest remaining-instructions)))
      sheet))
(length (fold-general (first (input 13 T)) (subseq (second (input 13 T)) 0 1)))
(fold-general (first (input 13 T)) (second (input 13 T)))

(defun print-folded-sheet (sheet)
  "Print the resulting code to a string."
  ;;THINK: max-x and max-y are probably constant.
  (let* ((max-x (loop for dot in sheet maximize (first dot)))
         (max-y (loop for dot in sheet maximize (second dot)))
         (output (make-array (list (1+ max-y) (1+ max-x)) :initial-element ".")))
    (loop for dot in sheet
          do (setf (aref output (second dot) (first dot))
                   "#"))
    ;;Manual "~{~{~A~}~^~%~}" format control string (works on arrays).
    (with-output-to-string (*standard-output*)
      (loop for line from 0 below (array-dimension output 0)
            do (progn (loop for col from 0 below (array-dimension output 1) do (princ (aref output line col)))
                      (terpri))))))
(print-folded-sheet (fold-general (first (input 13 T)) (second (input 13 T))))
(print-folded-sheet (fold-general (first (input 13 'full)) (second (input 13 'full))))

#|
I am unsure about the solution format required by the part2 variant.
It could be the result of #'print-folded-sheet (a big collection of dots and hashes) or it could be the parsed characters (8 bytes).
I think the latter holds true. This requires, as a matter of fact, some reverse engineering. Which is probably not intended...

Writing the generators for those puzzles seems like a nice additional exercise. Some generators seem pretty simple. For example, day 8 can randomly produce a key just by shuffling the segments.
|#

;;I gathered some inputs from the internet in my quest to produce a generally applicable solution to the/any puzzle 13.
(defparameter *code-letters*
  (list
   ;;A kludge for the example, does not collide with :c
   :example '("####"
              "#..."
              "#..."
              "#..."
              "####")
   :a '(".##."
        "#..#"
        "#..#"
        "####"
        "#..#"
        "#..#")
   :b '("###."
        "#..#"
        "###."
        "#..#"
        "#..#"
        "###.")
   :c '(".##."
        "#..#"
        "#..."
        "#..."
        "#..#"
        ".##.")
   :e '("####"
        "#..."
        "###."
        "#..."
        "#..."
        "####")
   :f '("####"
        "#..."
        "###."
        "#..."
        "#..."
        "#...")
   :g '(".##."
        "#..#"
        "#..."
        "#.##"
        "#..#"
        ".###")
   :h '("#..#"
        "#..#"
        "####"
        "#..#"
        "#..#"
        "#..#")
   :j '("..##"
        "...#"
        "...#"
        "...#"
        "#..#"
        ".##.")
   :k '("#..#"
        "#.#."
        "##.."
        "#.#."
        "#.#."
        "#..#")
   :l '("#..."
        "#..."
        "#..."
        "#..."
        "#..."
        "####")
   :p '("###."
        "#..#"
        "#..#"
        "###."
        "#..."
        "#...")
   :r '("###."
        "#..#"
        "#..#"
        "###."
        "#.#."
        "#..#")
   :u '("#..#"
        "#..#"
        "#..#"
        "#..#"
        "#..#"
        ".##.")
   :z '("####"
        "...#"
        "..#."
        ".#.."
        "#..."
        "####")))

(defrule sheet-lines (+ (and (+ (or "." "#")) (? new)))
  (:lambda (lines)
    (mapcar (lambda (x) (text (first x))) lines)))

(defun extract-letter (exploded-printed-sheet index)
  "Return the letter at position INDEX in EXPLODED-PRINTED-SHEET.
   Format of EXPLODED-PRINTED-SHEET is determined by the parsing rule 'sheet-lines."
  (mapcar (lambda (x)
            ;;5: width of a letter and its spacing.
            (let ((offset (* 5 index)))
              ;;4: width of a letter.
              (subseq x offset (+ offset 4))))
          exploded-printed-sheet))

(defun is-letter? (guess input)
  "GUESS: a key in *code-letters* (:a .. :z).
   INPUT: list of strings containing #\# or #\. representing a letter."
  (every #'string= (getf *code-letters* guess) input))
(is-letter? :h (let ((code (parse 'sheet-lines (print-folded-sheet (fold-general (first (input 13 'full)) (second (input 13 'full)))))))
                 (extract-letter code 0)))

(defun parse-letter (extracted-letter)
  "What letter EXTRACTED-LETTER represents (as string of length 1)."
  (or (loop for (k v) on *code-letters* by #'cddr
        when (is-letter? k extracted-letter)
          return (format nil "~A" k))
      (error "Please report the found unknown letter mapping to the author.~%I.e. send the input which produced:~%~%~{~A~^~%~}" extracted-letter)))
(let ((code (parse 'sheet-lines (print-folded-sheet (fold-general (first (input 13 'full)) (second (input 13 'full)))))))
  (parse-letter (extract-letter code 0)))

(defmethod solution ((day (eql 13)) (variant (eql 'part1)) source)
  "The number of dots after the first fold."
  (let ((input (input 13 source)))
    (length (fold-general (first input) (subseq (second input) 0 1)))))
(is 13 'part1 17)

(defmethod solution ((day (eql 13)) (variant (eql 'part2)) source)
  "The activation code."
  (let* ((input (input 13 source))
         (code (print-folded-sheet (fold-general (first input) (second input))))
         (code-prepared-for-extraction (parse 'sheet-lines code)))
    (format nil "~{~A~}"
            (loop for i from 0 below (if (or (eql source T)
                                             (eql source 'example))
                                         1
                                         8)
                  collect (parse-letter (extract-letter code-prepared-for-extraction i))))))
(is 13 'part2 "EXAMPLE")

