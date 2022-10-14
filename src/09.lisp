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

(defrule 09-line (+ (character-ranges (#\0 #\9)))
  (:lambda (input)
    (mapcar (lambda (y) (- (char-code y) 48)) input)))
(parse '09-line "1294")

(defrule 09-file (and (+ (and 09-line (? new))))
  (:lambda (input)
    (let ((entries (elt input 0)))
      ;;#'reverse: conveniently access elements with (aref array northing easting) (see #'map-lookup).
      (reverse (mapcar #'first entries)))))

(defmethod input ((day (eql 9)) source)
  "Multidimensional array representing the heightmap with heights [0..9]."
  (let* ((nested-lists (parse '09-file (read-file day source)))
         (height (length nested-lists))
         (width (length (first nested-lists))))
    (make-array (list height width) :initial-contents nested-lists)))
(input 9 T)
(input 9 'full)


;;;; Solution

(defun en2rmindex (map easting northing)
  "EASTING/NORTHING coordinate to row-major-index (rmindex) conversion.
   Reduces dimensionality by using the built-in row-order space filling curve."
  (array-row-major-index map northing easting))

(defparameter *4direction-vectors*
  '((0   (0 . 1))
    (90  (1 . 0))
    (180 (0 . -1))
    (270 (-1 . 0)))
  "Mathematical vectors for axis-parallel movement on a 2d grid. Realized as association lists, car is easting, cdr northing.")

(defun by-vector (direction &optional (lut *4direction-vectors*))
  "The mathematical vector in order to step towards a neighbour in DIRECTION [0..360].
   Look-up-table LUT defaults to the dictionary *4direction-vectors*."
  (cadr (assoc direction lut)))
(by-vector 0)

(defun easting (map rmindex)
  "rmindex to easting conversion."
  (mod rmindex (array-dimension map 1)))
(easting (input 9 T) 45)

(defun northing (map rmindex)
  "rmindex to northing conversion."
  (floor rmindex (array-dimension map 1)))
(northing (input 9 T) 45)

(defun rm-coord= (rmindex-a rmindex-b)
  "Whether the two rmindices are #'eq."
  (eq rmindex-a rmindex-b))
;;(rm-coord= '(1 . 3) '(1 . 3))
(rm-coord= 45 45)
(rm-coord= (en2rmindex (input 9 T) 1 3) (en2rmindex (input 9 T) 1 3))

(defun map-lookup (map rmindex)
  "Returns the value of the pixel at RMINDEX in MAP."
  (row-major-aref map rmindex))

(defun clip-move (map start by-vector)
  "Returns the new coordinates (as an rmindex) after adding BY-VECTOR to START or nil if the shift crosses the borders of the map."
  ;;TODO: optimize movement with simple additions of array widths etc.
  (let* ((width (array-dimension map 1))
         (height (array-dimension map 0))
         (new-easting (+ (easting map start) (car by-vector)))
         (new-northing (+ (northing map start) (cdr by-vector))))
    ;;Using common-lisp:and as control structure.
    (and (<= 0 new-easting (1- width))
         (<= 0 new-northing (1- height))
         (en2rmindex map new-easting new-northing))))
(clip-move (input 9 T) (en2rmindex (input 9 T) 2 3) '(1 . 1))

(defun direct-neighbour-values (map location)
  "Returns the values of the north/east/south/west neighbours for a given location on MAP denoted by EASTING and NORTHING.
   A neighbour outside map has the value nil.
   Format of MAP determined by #'input for this day.
   Coordinate system is rooted at the bottom left corner, EASTING shows right and NORTHING upwards."
  (flet ((move-by-vector (loc by)
           (let ((loc (clip-move map loc by)))
             (when loc
               (map-lookup map loc)))))
    (list 
     ;;North.
     (move-by-vector location (by-vector 0))
     ;;East.
     (move-by-vector location (by-vector 90))
     ;;South.
     (move-by-vector location (by-vector 180))
     ;;West.
     (move-by-vector location (by-vector 270)))))
;;Target: (8 8 NIL NIL).
(direct-neighbour-values (input 9 T) (en2rmindex (input 9 T) 0 0))
;;Target: (7 9 NIL 9).
(direct-neighbour-values (input 9 T) (en2rmindex (input 9 T) 1 0))
;;Target: (5 7 9 7).
(direct-neighbour-values (input 9 T) (en2rmindex (input 9 T) 2 1))
;;Target: (NIL 1 3 NIL).
(direct-neighbour-values (input 9 T) (en2rmindex (input 9 T) 0 4))

(defun low-point? (map location)
  "Whether the value at LOCATION is lower (#'<) than its four/three/two neighbours."
  (let* ((width (array-dimension map 1))
         (height (array-dimension map 0))
         (own-value (map-lookup map location))
         (neighbours (direct-neighbour-values map location))
         ;;Windrose of booleans whether direction is outside of map.
         (neighbour-outside-anyway? (list
                                     ;;North.
                                     (= height (1+ (northing map location)))
                                     ;;East.
                                     (= width (1+ (easting map location)))
                                     ;;South.
                                     (= 0 (northing map location))
                                     ;;West.
                                     (= 0 (easting map location)))))
    (loop for neighbour in neighbours
          for outside-anyway? in neighbour-outside-anyway?
          always (or outside-anyway? (< own-value neighbour)))))
(and (low-point? (input 9 T) (en2rmindex (input 9 T) 1 4))
     (low-point? (input 9 T) (en2rmindex (input 9 T) 9 4))
     (low-point? (input 9 T) (en2rmindex (input 9 T) 2 2))
     (low-point? (input 9 T) (en2rmindex (input 9 T) 6 0)))

(defun low-points (map)
  "Return all points on MAP whose neighbours are higher.
   Point format: rmindex."
  (let ((width (array-dimension map 1))
        (height (array-dimension map 0)))
    (loop for easting from 0 below width
          append
          (loop for northing from 0 below height
                when (low-point? map (en2rmindex map easting northing))
                  collect (en2rmindex map easting northing)))))
(low-points (input 9 T))
(low-points (input 9 'full))

(defun risk-sum (map)
  "For each low point in map, add its risk level, which is defined as being height+1, to the returned value."
  (loop for coord in (low-points map)
        summing (1+ (map-lookup map coord))))
(risk-sum (input 9 T))

(defmethod solution ((day (eql 9)) (variant (eql 'part1)) source)
  "Sum all risk levels in the map (the input)."
  (risk-sum (input day source)))
(is 9 'part1 15)

#|
Part2 solution: flood fill algorithm.

Caves a b c d:

 a,3
 ↓
+----------+
|  XXX     |←b,9
| X   X X  |
|X     X X |
|     X   X|
|X XXX     |←d,9
+----------+
  ↑
  c,14
|#

(defun valid-neighbours (map coord)
  "Returns a list of neighbours (as rmindex) which do not have height 9."
  (when coord
    (remove
     nil
     (loop for (dir . (vector)) in *4direction-vectors*
           collect (let* ((neighbour (clip-move map coord vector))
                          (n-value (when neighbour
                                     (map-lookup map neighbour))))
                     (when (and n-value (/= 9 n-value))
                       neighbour))))))
;;Top left basin of size 3.
(valid-neighbours (input 9 T) (en2rmindex (input 9 T) 1 4))
(valid-neighbours (input 9 T) (en2rmindex (input 9 T) 0 4))
(valid-neighbours (input 9 T) (en2rmindex (input 9 T) 0 3))
(valid-neighbours (input 9 T) nil)

(defun flood-fill (map start-coord visited-cells)
  "Flood fill.
   Determine valid neighbours. If valid (value < 9), add neighbour to VISITED-CELLS in recursive call.
   If no further fresh possibilites, return VISITED-CELLS.
   Expects *visited-cells-map*."
  ;;Pure functional style took prohibitive 10s for a basin of size 83 (statistical profiling showed that the bottleneck was #'rm-coord= for (subsetp valid-neighbours visited-cells) in unvisited-neighbours in the let* below).
  (declare (special *visited-cells-map*))
  (let* ((valid-neighbours (valid-neighbours map start-coord))
         (unvisited-neighbours (loop for n in valid-neighbours
                                     unless (or (row-major-aref *visited-cells-map* n)
                                                (member n visited-cells))
                                       collect n)))
    (if unvisited-neighbours
        (loop for n in unvisited-neighbours
              do (loop for cell in (flood-fill map
                                               n
                                               (append unvisited-neighbours visited-cells))
                       do (setf (row-major-aref *visited-cells-map* cell) T)))
        visited-cells)))

(defun basin-size (map start-coord)
  "The size of a basin of which START-COORD is member of."
  (let ((*visited-cells-map* (make-array (array-dimensions map) :initial-element nil)))
    (declare (special *visited-cells-map*))
    (flood-fill map start-coord nil)
    (loop for i from 0 below (array-total-size *visited-cells-map*)
          count (row-major-aref *visited-cells-map* i))))
(basin-size (input 9 T) (en2rmindex (input 9 T) 1 4))
(basin-size (input 9 T) (en2rmindex (input 9 T) 9 4))
(basin-size (input 9 T) (en2rmindex (input 9 T) 2 2))
(basin-size (input 9 T) (en2rmindex (input 9 T) 6 0))

(defun top-3-basins-product (map)
  "For each random starting position (determined by #'low-points), apply the flood-fill algorithm.
   Return the multiplied sizes of the biggest three floods.
   Returns all possible flood fills since all low-points were determined beforehand.
   No basin is determined twice (this is verified)."
  (let* ((basins (mapcar (lambda (x) (basin-size map x))
                         (low-points map)))
         (sizes-sorted (sort basins #'>)))
    (* (or (first sizes-sorted) 1)
       (or (second sizes-sorted) 1)
       (or (third sizes-sorted) 1))))
(top-3-basins-product (input 9 'full))

(defmethod solution ((day (eql 9)) (variant (eql 'part2)) source)
  "Multiply the sizes of the three biggest basins in the map (the input)."
  (top-3-basins-product (input day source)))
(is 9 'part2 1134)

