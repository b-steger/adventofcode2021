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

(defun seafloorascii2bit (char)
  "Converts #\# => 1 and #\. => 0."
  (if (string= char "#") 1 0))

(defrule enhancement-lut (+ (or #\. #\#))
  (:lambda (input)
    (make-array (list (length input)) :element-type 'bit :initial-contents (mapcar #'seafloorascii2bit input))))
(simple-bit-vector-p (parse 'enhancement-lut "..#.#..##"))

(defrule seafloor-map (+ (and (+ (or #\. #\#)) (? new)))
  (:lambda (input) (mapcar (lambda (x) (mapcar #'seafloorascii2bit x))
                           (mapcar #'first input)))
  (:lambda (input)
      (make-array (list (length input) (length (first input)))
                  :element-type 'bit
                  ;;Reversing because of northing, see parser rule 09-file in 09.lisp.
                  :initial-contents (reverse input))))
(parse 'seafloor-map "#..#.
#....
##..#
..#..
..###")

(defrule 20-file (and enhancement-lut new new seafloor-map)
  (:lambda (input)
    (let ((lut (elt input 0))
          (map (elt input 3)))
      (list lut map))))
(parse '20-file (read-file 20 'example))

(defmethod input ((day (eql 20)) source)
  "An enhancement lookup table (simple bit vector) and a seafloor map (multidimensional array of bits)."
  (parse '20-file (read-file 20 source)))
(input 20 T)
(input 20 'full)


;;;; Solution

#|
There is the possibility that a pixel outside of the map is lit.
Which means that the map must be able to accept a buffer of one pixel per step.
For 2 steps, a buffer of 2 is enough. For 50 50.
|#

(defun buffered-map (map buffer initvalue)
  "Surround the rectangle MAP with zeroes, i.e. copy an array in the middle of a new one with widths/heights increased by (* 2 BUFFER)."
  (let ((width (array-dimension map 1))
        (height (array-dimension map 0))
        (new-map (make-array (mapcar (lambda (x) (+ (* buffer 2) x)) (array-dimensions map))
                             :element-type 'bit
                             ;;See #'enhance-image.
                             :initial-element initvalue)))
    (loop for northing from 0 below height
          do (loop for easting from 0 below width
                   do (setf (aref new-map (+ buffer northing) (+ buffer easting)) (aref map northing easting))))
    new-map))
(buffered-map (second (input 20 T)) 2 0)
(neighbour-lut (buffered-map (second (input 20 T)) 2 0)
               :lut *8direction-vectors*
               :positional-neighbours? T)

(defun enhance-image (enhancement-lut map &key (steps-after 1))
  "Return a new array which is larger since it is buffered with a buffer of size 1. Additionally, every rmindex is the result of a moving window function."
  (let* ((default (if (and (= 1 (aref enhancement-lut 0))
                           (= 0 (aref enhancement-lut (1- (array-dimension enhancement-lut 0)))))
                      ;;Steer against flipping. This is needed even though the requested steps are even - the border areas get affected by the surrounding open land.
                      (- 1 (rem steps-after 2))
                      0))
         (buffered-source-map (buffered-map map 1 default))
         ;;Can use any input (of correct size) since all values will get overriden.
         (buffered-result-map (buffered-map map 1 default))
         (neighbour-lut (neighbour-lut buffered-source-map
                                       :lut *8direction-vectors*
                                       :positional-neighbours? T)))
    (loop for i from 0 below (array-total-size buffered-source-map)
          do (setf
              (row-major-aref buffered-result-map i)
              (aref
               enhancement-lut
               (to-decimal
                (let ((neighbours
                        (make-array
                         ;;8: number of neighbours in a 2d map.
                         '(8)
                         :initial-contents (mapcar (lambda (x)
                                                     (if x
                                                         (row-major-aref buffered-source-map x)
                                                         default))
                                                   (row-major-aref neighbour-lut i)))))
                  ;;The indices 0..8 of the spec refer to directions 315 0 45 270 i 90 225 180 135, respectively.
                  (list
                   (aref neighbours 7)
                   (aref neighbours 0)
                   (aref neighbours 1)
                   (aref neighbours 6)
                   ;;The value itself.
                   (row-major-aref buffered-source-map i)
                   (aref neighbours 2)
                   (aref neighbours 5)
                   (aref neighbours 4)
                   (aref neighbours 3)))))))
    (if (= 0 steps-after)
        buffered-result-map
        (enhance-image enhancement-lut buffered-result-map :steps-after (1- steps-after)))))
(let* ((input (input 20 T))
       (output (enhance-image (first input) (second input) :steps-after 1)))
  (format nil "~A"
          (with-output-to-string (*standard-output*)
            (loop for line from (1- (array-dimension output 0)) downto 0
                  do (progn (loop for col from 0 below (array-dimension output 1) do (princ (if (= 0 (aref output line col))
                                                                                                "." "#")))
                            (terpri))))))

(defun solution-20 (source part2?)
  "Enhance the map and count the number of light pixels."
  (let* ((input (input 20 source))
         (enhanced (enhance-image (first input) (second input)
                                  :steps-after (if part2?
                                                   49
                                                   1))))
    (loop for i from 0 below (array-total-size enhanced)
          sum (row-major-aref enhanced i))))

(defmethod solution ((day (eql 20)) (variant (eql 'part1)) source)
  "The number of light pixels after 2 image enhancement steps."
  (solution-20 source nil))
(is 20 'part1 35)

(defmethod solution ((day (eql 20)) (variant (eql 'part2)) source)
  "The number of light pixels after 50 image enhancement steps."
  (solution-20 source T))
(is 20 'part2 3351)

