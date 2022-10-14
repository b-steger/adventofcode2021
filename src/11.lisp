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

(defmethod input ((day (eql 11)) source)
  "Multidimensional array representing the energy level of the octopuses [0..9].
   No zero is read in. Ever."
  (let* ((nested-lists (parse '09-file (read-file day source)))
         (height (length nested-lists))
         (width (length (first nested-lists))))
    (make-array (list height width) :initial-contents nested-lists)))
(input 11 T)
(input 11 'toy-example)
(input 11 'full)


;;;; Solution

(defparameter *8direction-vectors*
  '((0   (0  . 1))
    (45  (1  . 1))
    (90  (1  . 0))
    (135 (1  . -1))
    (180 (0  . -1))
    (225 (-1 . -1))
    (270 (-1 . 0))
    (315 (-1 . 1)))
  "Mathematical vectors for horizontal/vertical/diagonal movement on a 2d grid. Realized as association lists, car is easting, cdr northing.")

(defun flash-count (map location)
  "Propagate flashes from octopuses in a cavern and count the number of flashes.
   MAP is closed over."
  ;;Need to reset values >9 to 0 as the last action during a step.
  (incf (row-major-aref map location))
  ;;10: 9 when entering this function.
  (cond ((= 10 (map-lookup map location))
         ;;1+: current octopus just flashed.
         (1+ (loop for (dir . (vector)) in *8direction-vectors*
                   summing (let ((neighbour (clip-move map location vector)))
                             (if neighbour
                                 (flash-count map neighbour)
                                 0)))))
        (T 0)))

(defun step-octopuses! (map &key (steps 100) part2? return-map?)
  "Simulate a step in the octopus cavern and return the number of flashes after STEPS steps if PART2? is nil.
   PART2? has precedence over STEPS.
   RETURN-MAP? is for internal use only."
  (loop with flashes = 0
        with step = 0
        while (if part2?
                  (not (loop for i from 0 below (array-total-size map)
                             always (= 0 (row-major-aref map i))))
                  (< step steps))
        do (progn
             ;;1. Propagate flashes.
             (incf flashes
                   (loop for i from 0 below (array-total-size map)
                         summing (flash-count map i )))
             ;;2. Set values >9 to 0.
             (loop for i from 0 below (array-total-size map)
                   do (when (< 9 (row-major-aref map i))
                        (setf (row-major-aref map i) 0)))
             (incf step))
        finally (return (cond (return-map? map)
                              (part2? step)
                              (T flashes)))))
(step-octopuses! (input 11 'toy-example) :steps 2 :return-map? T)
(step-octopuses! (input 11 T))
(step-octopuses! (input 11 'full))
(step-octopuses! (input 11 T) :part2? T)

(defmethod solution ((day (eql 11)) (variant (eql 'part1)) source)
  "The number of flashes after 100 steps."
  (step-octopuses! (input day source) :steps 100))
(is 11 'part1 1656)

(defmethod solution ((day (eql 11)) (variant (eql 'part2)) source)
  "The step when all octopuses flash simultaneously."
  (step-octopuses! (input day source) :part2? T))
(is 11 'part2 195)

