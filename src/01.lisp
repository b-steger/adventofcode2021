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

(defrule 01-file (+ (and integer (? new)))
  (:lambda (input)
    (mapcar #'first input)))

(defmethod input ((day (eql 1)) source)
  (parse '01-file (read-file day source)))
(input 1 T)


;;;; Solution

(defun count-increasing (list &optional lagging)
  "The number of increasing values in LIST."
  (if list
      (+ (if (< (or lagging (first list)) (first list)) 1 0)
         (count-increasing (rest list) (first list)))
      0))

(defmethod solution ((day (eql 1)) (variant (eql 'part1)) source)
  "Count the number of times a value in a sequence is increasing."
  (count-increasing (input 1 source)))
(is 1 'part1 7)

(defun summing-moving-window (list)
  "Recursive moving window which sums three numbers."
  (cons (+ (first list)
           (second list)
           (third list))
        (when (cdddr list)
          (summing-moving-window (rest list)))))
(summing-moving-window (input 1 T))

(defmethod solution ((day (eql 1)) (variant (eql 'part2)) source)
  "Part 1, but with a summing moving window of size three passing over the input."
  (count-increasing (summing-moving-window (input 1 source))))
(is 1 'part2 5)

