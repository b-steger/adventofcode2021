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

(defmethod input ((day (eql 7)) source)
  "Horizontal positions of crab submarines."
  ;;Reusing format parsed by rule '06-file.
  (parse '06-file (read-file day source)))
(input 7 T)
(input 7 'full)


;;;; Solution

;;Problem space is small (1878).
(loop for x in (input 7 'full)
      maximize x)

(defun 07-cost-part1 (source target)
  "The distance between SOURCE and TARGET."
  (abs (- source target)))
(mapcar (lambda (x) (07-cost-part1 x 2)) (input 7 T))

(defun 07-cost (input position &key (cost-function #'07-cost-part1))
  "The sum of costs each number has in relation to POSITION."
  (reduce #'+ (mapcar (lambda (x) (funcall cost-function x position)) input)))
;;=> (37 41 39 71)
(list (07-cost (input 7 T) 2)
      (07-cost (input 7 T) 1)
      (07-cost (input 7 T) 3)
      (07-cost (input 7 T) 10))

(defun cheapest-fuel (input positions cost-function)
  "Simple case: calculate the cost to reach position with COST-FUNCTION.
   Recursive case: smaller of
                   simple case on the first position on a list
                   - pitted against -
                   the recursive call on the rest of the list."
  (if (cdr positions)
      (min (cheapest-fuel input (list (car positions)) cost-function)
           (cheapest-fuel input (rest positions) cost-function))
      (07-cost input (car positions)
               :cost-function cost-function)))

(defun 07-solution (day source cost-function)
  "Wrapper around #'cheapest-fuel."
  (cheapest-fuel (input day source)
                 (loop for x below (loop for x in (input day source) maximize x)
                       collect x)
                 cost-function))
;;=> 37
(07-solution 7 T #'07-cost-part1)

(defun 07-cost-part2 (source target)
  "The area of the triangle determined by (0 0)--(diff+1 0)--(diff+1 diff)."
  (let ((diff (07-cost-part1 source target)))
    (/ (* (1+ diff) diff) 2)))
;;=> (168 206)
(list (07-cost (input 7 T) 5 :cost-function #'07-cost-part2)
      (07-cost (input 7 T) 2 :cost-function #'07-cost-part2))

(defmethod solution ((day (eql 7)) (variant (eql 'part1)) source)
  "The least amount of fuel the linear cost crab submarines have to spend in order to align."
  (07-solution day source #'07-cost-part1))
(is 7 'part1 37)

(defmethod solution ((day (eql 7)) (variant (eql 'part2)) source)
  "The least amount of fuel the increasing cost crab submarines have to spend in order to align."
  (07-solution day source #'07-cost-part2))
(is 7 'part2 168)

