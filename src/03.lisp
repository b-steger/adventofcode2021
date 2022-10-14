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

(defrule binary-number (and (+ (or #\0 #\1)))
  (:lambda (input)
    (let ((number (elt input 0)))
      (mapcar (lambda (x) (if (string= "1" x) 1 0))
              number))))
(parse 'binary-number "00100")
(parse 'binary-number "111010111011")

(defrule 03-file (+ (and binary-number (? new)))
  (:lambda (input)
    (mapcar #'first input)))

(defmethod input ((day (eql 3)) source)
  (parse '03-file (read-file day source)))
(input 3 T)
(input 3 'full)


;;;; Solution

(defun to-decimal (bit-list)
  "Convert a list of bits (0 or 1) to an integer."
  (parse-integer (format nil "~{~A~}" bit-list) :radix 2))

;;Can use (- 1 (most-common-bit ...)) for the inverse as mandated by the spec. 
(defun most-common-bit (input position)
  "Returns the most common bit in the column determined by POSITION. INPUT is like puzzle input.
   Returns 1 if equally common."
  ;;THINK: #'logcount or #'logxor/#'logbitp with #'mask-field along "vertical numbers"? → Lists are good enough.
  (if (< (loop for i in input
               sum (elt i position))
         (/ (length input) 2))
      0
      1))
(most-common-bit (input 3 T) 0)
(most-common-bit '((0) (0) (1) (1)) 0)

(defmethod solution ((day (eql 3)) (variant (eql 'part1)) source)
  "Choose the most common bit of a column, interpret those statistics as integer (called gamma) and additionally bitwise-invert it (called epsilon)."
  (let* ((input (input 3 source))
         (integer-size (length (first input)))
         (gamma-binary (loop for pos from 0 below integer-size
                             collect (most-common-bit input pos)))
         (epsilon-binary (loop for x in gamma-binary collect (- 1 x)))
         (gamma-decimal (to-decimal gamma-binary))
         (epsilon-decimal (to-decimal epsilon-binary)))
    ;;Power consumption.
    (* gamma-decimal epsilon-decimal)))
(is 3 'part1 198)

(defun rating (&key input pos scrubber?)
  "Filter for numbers in INPUT which have
    the most common bit across all columns at POS if scrubber? is nil (called O2 rating).
    the least common bit across all columns at POS if scrubber? is non-nil (called scrubber rating)."
  (if (cdr input)
      (rating
       :input (remove-if-not
               (lambda (x)
                 (= (if scrubber?
                        (- 1 (most-common-bit input pos))
                        (most-common-bit input pos))
                    (elt x pos)))
               input)
       :pos (1+ pos)
       :scrubber? scrubber?)
      (car input)))
(rating :input (input 3 T) :pos 0)
(rating :input (input 3 T) :pos 0 :scrubber? T)

(defmethod solution ((day (eql 3)) (variant (eql 'part2)) source)
  "The product of the oxygen generator rating and the CO2 scrubber rating."
  (let ((input (input day source)))
    (* (to-decimal (rating :input input :pos 0))
       (to-decimal (rating :input input :pos 0 :scrubber? T)))))
(is 3 'part2 230)

