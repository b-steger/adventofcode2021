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

(defrule 05-coord (and integer "," integer)
  (:lambda (input)
    (let ((x (elt input 0))
          (y (elt input 2)))
      (list x y))))
(parse '05-coord "123,456")

(defrule 05-line (and 05-coord " -> " 05-coord)
  (:lambda (input)
    (let ((start (elt input 0))
          (end (elt input 2)))
      (list start end))))
(parse '05-line "0,9 -> 5,9")

(defrule 05-file (and (+ (and 05-line (? new))))
  (:lambda (input)
    (let ((entries (elt input 0)))
      (mapcar #'first entries))))

(defmethod input ((day (eql 5)) source)
  "List of lines. A line is a list of start and end coordinates. A coordinate is a list of x and y."
  (parse '05-file (read-file day source)))
(input 5 T)
(input 5 'full)


;;;; Solution

(defun coord (x y)
  "Construct a coordinate."
  (list x y))

(defun x (coordinate)
  "The position on the x axis of a coordinate."
  (first coordinate))

(defun y (coordinate)
  "The position on the y axis of a coordinate."
  (second coordinate))

(defun coord= (coordinate-1 coordinate-2)
  (and (= (x coordinate-1) (x coordinate-2))
       (= (y coordinate-1) (y coordinate-2))))

(defun offset (coordinate by-x by-y)
  "Adds BY-X and BY-Y to COORDINATE."
  (list (+ (first coordinate) by-x)
        (+ (second coordinate) by-y)))
(offset (list 1 2) 3 4)

(defun line (start end)
  "Construct a line."
  (list start end))

(defun start (line)
  "The starting coordinate of a LINE."
  (first line))

(defun end (line)
  "The ending coordinate of a LINE."
  (second line))

(defun direction (number)
  "Whether a number is positive (return 1), zero (return 0) or negative (return -1)."
  (cond ((minusp number) -1)
        ((zerop number) 0)
        (T 1)))
(mapcar #'direction '(-3 0 3))

(defun drawn-pixels (line &optional diagonal-forbidden?)
  "Returns the coordinates a LINE (format see parser rule '05-line) covers.
   Contract: LINE has the same x, same y or the same difference."
  (let* ((start (start line))
         (end (end line))
         (dist-x (- (x end) (x start)))
         (dist-y (- (y end) (y start))))
    (unless (and diagonal-forbidden? (not (zerop dist-x)) (not (zerop dist-y)))
      (if (coord= start end)
          (list end)
          (cons start (drawn-pixels (line (offset start
                                                  (direction dist-x)
                                                  (direction dist-y))
                                          (second line))))))))
(drawn-pixels '((0 0) (4 0)) T)
(drawn-pixels '((0 0) (4 0)))
(drawn-pixels '((0 0) (0 4)))
(drawn-pixels '((0 0) (5 5)))
(drawn-pixels '((0 0) (5 5)) T)
(drawn-pixels '((0 0) (-5 5)))
(drawn-pixels '((0 0) (-5 5)) T)
(mapcar (lambda (x) (drawn-pixels x T)) (input 5 T))
(drawn-pixels (elt (input 5 T) 0) T)

(defun 05-solution (day source diagonal-forbidden?)
  "Draw pixels and tally them."
  (let ((covered-pixels (loop for line in (input day source)
                               append (drawn-pixels line diagonal-forbidden?)))
         (tally (make-hash-table :test 'equal)))
    (loop for pixel in covered-pixels
          do (setf (gethash pixel tally)
                   (if (gethash pixel tally)
                       (incf (gethash pixel tally))
                       1)))
    (loop for entry being the hash-value of tally
          when (> entry 1)
            counting entry)))

(defmethod solution ((day (eql 5)) (variant (eql 'part1)) source)
  "The number of intersections of axis-aligned hydrothermal vent groups."
  (05-solution day source T))
(is 5 'part1 5)

(defmethod solution ((day (eql 5)) (variant (eql 'part2)) source)
  "The number of intersections of diagonal and axis-aligned hydrothermal vent groups."
  (05-solution day source nil))
(is 5 'part2 12)

