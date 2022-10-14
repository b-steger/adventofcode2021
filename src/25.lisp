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

(defrule sea-cucumber (or #\> #\v #\.)
  (:lambda (input)
    (cond ((string= ">" input) '>)
          ((string= "v" input) 'v)
          (T '+))))

(defrule 25-line (+ sea-cucumber)
  (:lambda (input)
    input))
(parse '25-line "...>>>>>...")

(defrule 25-file (and (+ (and 25-line (? new))))
  (:lambda (input)
    (let ((entries (elt input 0)))
      ;;#'reverse: northing, see puzzle 9.
      (reverse (mapcar #'first entries)))))

(defmethod input ((day (eql 25)) source)
  "Multidimensional array representing the sea floor of the Mariana trench. Values are '> for horizontally-moving sea cucumbers and 'v for vertically-moving ones, '+ for the empty sea floor."
  (let* ((nested-lists (parse '25-file (read-file day source)))
         (height (length nested-lists))
         (width (length (first nested-lists))))
    (make-array (list height width) :initial-contents nested-lists)))
(input 25 'toy-example)
(input 25 'middle-example)
(input 25 T)
(input 25 'full)


;;;; Solution

#|
Additional data structures such as ... interweaved skip lists won't help a lot here.
|#

;;See #'wrapping-move which has to be placed before #'neighbour-lut in 15.lisp.

(defun draw-map (map &optional (stream *standard-output*))
  "Draw the MAP onto STREAM.
   Returns a string if the STREAM is nil."
  (with-output-to-string (out)
    (format (or stream out) "~%")
    (loop for northing downfrom (1- (array-dimension map 0)) downto 0
          do (loop for easting from 0 below (array-dimension map 1)
                   do (format (or stream out) "~A" (aref map northing easting))
                   finally (format (or stream out) "~%")))))
(draw-map (input 25 'toy-example) nil)

(defun forage (map neighbours &key (steps nil) (counter 1) (wished-result 'counter))
  "Simulates two herds of sea cucumbers and returns - as the default - the number of steps before all animals stop moving.
   If STEPS is set, only simulate STEPS steps.
   Valid values for WISHED-RESULT: 'counter or 'map.
   COUNTER is for internal use only.
   (Reasoning for the initialization to 1: the first call is the first step.)"
  (if (and steps (= 0 steps))
      ;;1+: the current invocation itself.
      (if (eql wished-result 'counter) counter map)
      (let (
            ;;The next two map the current-index → T. An entry means that an animal has reached this place.
            (>-moved (make-hash-table))
            (v-moved (make-hash-table))
            ;;The next two map the current-index → T. An entry means that an animal was at this place. Is only relevant in the respective group loop.
            (>-occupied (make-hash-table))
            (v-occupied (make-hash-table)))
        (flet ((animal-move (own-index next-index group)
                 (when (and (eql '+ (row-major-aref map next-index))
                            (not (gethash next-index (if (eql '> group) >-occupied v-occupied))))
                   (setf
                    ;;Remember where the animal was.
                    (gethash own-index (if (eql '> group) >-occupied v-occupied)) T
                    ;;Block the place the animal has moved to.
                    (gethash next-index (if (eql '> group) >-moved v-moved)) T
                    (row-major-aref map own-index) '+
                    (row-major-aref map next-index) group))))
          (loop for i from 0 below (array-total-size map)
                when (and (eql '> (row-major-aref map i)) (not (gethash i >-moved)))
                  do (animal-move i (second (row-major-aref neighbours i)) '>))
          (loop for i from 0 below (array-total-size map)
                when (and (eql 'v (row-major-aref map i)) (not (gethash i v-moved)))
                  do (animal-move i (third (row-major-aref neighbours i)) 'v))
          (forage map neighbours
                  :steps
                  ;;Check whether any animal can move in the next step.
                  (if (loop for i from 0 below (array-total-size map)
                            when (and (eql '> (row-major-aref map i))
                                      (eql '+ (row-major-aref map (second (row-major-aref neighbours i)))))
                              return T
                            when (and (eql 'v (row-major-aref map i))
                                      (eql '+ (row-major-aref map (third (row-major-aref neighbours i)))))
                              return T)
                      (and steps (1- steps))
                      0)
                  :counter (1+ counter)
                  :wished-result wished-result)))))
(draw-map (forage (input 25 'toy-example)
                  (neighbour-lut (input 25 'toy-example) :wrapping-is-allowed? T)
                  :steps 1
                  :wished-result 'map)
          nil)
(draw-map (forage (input 25 'middle-example)
                  (neighbour-lut (input 25 'middle-example) :wrapping-is-allowed? T)
                  :steps 4
                  :wished-result 'map)
          nil)

(defmethod solution ((day (eql 25)) (variant (eql 'part1)) source)
  "Number of steps before the two herds of sea cucumbers stop moving while foraging for food."
  (forage (input day source)
          (neighbour-lut (input day source) :wrapping-is-allowed? T)
          :steps nil
          :wished-result 'counter))
(is 25 'part1 58)

;;Defined since the report does not compute applicable methods.
(defmethod solution ((day (eql 25)) (variant (eql 'part2)) source)
  "The last star is a present."
  (declare (ignorable day variant source))
  0)
(is 25 'part2 0)

