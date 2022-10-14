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

(defrule 06-file (and (+ (and integer (? ",")))
                      (? new))
  (:lambda (input)
    (let ((entries (elt input 0)))
      (mapcar #'first entries))))

(defmethod input ((day (eql 6)) source)
  "List of fish timers."
  (parse '06-file (read-file day source)))
(input 6 T)
(input 6 'full)


;;;; Solution

#|
There seems no need to represent each fish in an instance because all fishes with the same timer have the same state change.
As the part2 task hints, there will be very deep recursions if the suggested approach (returning a sum with doubly called descending steps for all fishes in question) is taken.
A giant array may work (as of 2022), even though space use would be superlinear.
As already said, since state changes can be aggregated, a histogram seems to be the most scalable approach.
|#

(defun timer-histogram (input &optional (histogram (list 0 0 0 0 0 0 0 0 0)))
  "Returns a histogram of the numbers in INPUT, represented as (zero-indexed) list."
  (cond (input (incf (elt histogram (car input)))
               (timer-histogram (rest input) histogram))
        (T histogram)))
(timer-histogram (input 6 T))

#|
Next, following observation: the number of children is exactly the number of parents.
Which leads to thought reversal: at the modulo border, new adult fishes (of timer 6) are introduced, not children.

During a step, the population pyramid shifts by one.
Slice 6 sees the introduction of new members.
|#

(defun 06-step (days-left histogram &optional (pointer 0))
  "Shift HISTOGRAM (achieved though the recursive increase of POINTER) until DAYS-LEFT equals to zero, returning the sum of the population pyramid."
  (cond ((zerop days-left) (reduce #'+ histogram))
        (T (06-step (1- days-left)
                    (progn
                      ;;7: timer 6 zero-indexed
                      (incf (elt histogram (mod (+ pointer 7) (length histogram)))
                            (elt histogram pointer))
                      histogram)
                    (mod (1+ pointer) (length histogram))))))
;;26
(06-step 18 (timer-histogram (input 6 T)))
;;5934
(06-step 80 (timer-histogram (input 6 T)))
;;26984457539
(06-step 256 (timer-histogram (input 6 T)))
#|After around 7036 years, the decimal number needs 97236 digits to represent the population size.
(with-open-file (f "/dev/shm/06-fishes.txt" :if-exists :supersede :if-does-not-exist :create :direction :output)
  (format f "~A" (06-step 2569999 (timer-histogram (input 6 T)))))

After 25699999 days (approx. 70363 years), the number has 972348 digits.|#

(defmethod solution ((day (eql 6)) (variant (eql 'part1)) source)
  "Population size of lanternfish after 80 days."
  (06-step 80 (timer-histogram (input day source))))
(is 6 'part1 5934)

(defmethod solution ((day (eql 6)) (variant (eql 'part2)) source)
  "Population size of lanternfish after 256 days."
  (06-step 256 (timer-histogram (input day source))))
(is 6 'part2 26984457539)

