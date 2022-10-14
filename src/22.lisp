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

(defrule reboot-step (and (or "on" "off") " "
                          "x=" integer-z ".." integer-z ","
                          "y=" integer-z ".." integer-z ","
                          "z=" integer-z ".." integer-z)
  (:lambda (input)
    (let ((set (elt input 0))
          (xmin (elt input 3))
          (xmax (elt input 5))
          (ymin (elt input 8))
          (ymax (elt input 10))
          (zmin (elt input 13))
          (zmax (elt input 15)))
      (list
       :set-value (if (string= "on" set) T nil)
       :xmin xmin
       :xmax xmax
       :ymin ymin
       :ymax ymax
       :zmin zmin
       :zmax zmax))))

(defrule 22-file (+ (and reboot-step (? new)))
  (:lambda (input)
    (mapcar #'first input)))

(defmethod input ((day (eql 22)) source)
  "Reactor reboot steps, each listing the SET-VALUE and the max/min object axis values which denote the affected reactor core area."
  (parse '22-file (read-file 22 source)))
(input 22 'toy-example)
(input 22 'middle-example)
(input 22 'example)
(input 22 'full)

(defun 22-part1-filter (input)
  "Filter for reboot steps which are fully within the interval [-50..50] on each axis.
   Also see #'22-input."
  (remove-if-not
   (lambda (x)
     (and (<= -50 (getf x :xmin) (getf x :xmax) 50)
          (<= -50 (getf x :ymin) (getf x :ymax) 50)
          (<= -50 (getf x :zmin) (getf x :zmax) 50)))
   input))
(22-part1-filter (input 22 'toy-example))
(22-part1-filter (input 22 'middle-example))
(22-part1-filter (input 22 'example))
(22-part1-filter (input 22 'full))


;;;; Solution

#|
The topic of this puzzle is the rectangle intersection & measure problem, which boils down to the space / object decomposition problem. See "Foundations of Multidimensional and Metric Data Structures. Hanan Samet. 2006. Published by Morgan Kaufmann, an Imprint of Elsevier. San Francisco", section 3.1. "Plane-Sweep Methods and the Rectangle Intersection Problem" (p. 428ff) and section 3.2. "Plane-Sweep Methods and the Measure Problem" (p. 447ff).

The closest match is the exercise 4 of the section 3.2. (p. 452). The solution (p. 823) is essentially a reference to "J. van Leeuwen and D. Wood. The measure problem for rectangular ranges in d-space. Journal of Algorithms, 2(3):282-300, September 1981".

I do not implement a region quadtree for a sweeping plane, nor do I use segment / interval trees (Bentley's approach). I recombine some of the presented ideas in a puzzle-tailored fashion instead. I will perform an adapted octree insertion whilst pursuing a data-oriented space decomposing strategy. See section 2.1.2.3. "Irregular Grid" (p. 210f) and section 2.1.2.4. "Region Quadtree and Region Octree" (p. 211ff).

Naming convention: the "input space" is the unmodified, uniform space of the input. The translated grid space which maps observed axis-projected interval endpoints to the input space is called "irregular space".
|#

#|
The mapping to the input space is governed by a lookup table.
|#

(defun irregular-grid-lut (input)
  "Returns all positions of all hyperplanes in INPUT.
   Includes the position just right to every interval, not the max value itself.
   Returns a list for each axis (x y z) with an array each.
   Also known as 'linear scales'."
  (flet ((axis-values (input min-name max-name)
           "Returns all hyperplane positions (as an array)."
           (let* ((unique-and-sorted-positions
                    (remove-duplicates
                     (sort (loop for step in input
                                 collect (getf step min-name)
                                 ;;Intervals are open ended on the right side.
                                 collect (1+ (getf step max-name)))
                           #'<))))
             (make-array (length unique-and-sorted-positions)
                         :element-type 'fixnum
                         :initial-contents unique-and-sorted-positions))))
    (list (axis-values input :xmin :xmax)
          (axis-values input :ymin :ymax)
          (axis-values input :zmin :zmax))))
(length (first (irregular-grid-lut (input 22 'full))))

(defun xyz2irregular (lut x y z)
  "Input space → irregular space conversion."
  (list (position x (first lut))
        (position y (second lut))
        (position z (third lut))))
(xyz2irregular (irregular-grid-lut (input 22 'middle-example)) -54112 41 53682)

(defun irregular2xyz (lut x y z)
  "Irregular space → input space conversion."
  (list (aref (first lut) x)
        (aref (second lut) y)
        (aref (third lut) z)))
(irregular2xyz (irregular-grid-lut (input 22 'middle-example)) 0 28 37)

(defun 22-input (source &key irregular-space? optimize? part1? reverse-time?)
  "Input of day 22 in customized configurations.
   If IRREGULAR-SPACE? is non-nil, the input is returned in irregular space, not in input space (the input space is the default).
   If OPTIMIZE? is non-nil, the input is retuned as an array (0:xmin 1:xmax ... 5:zmax 6:set-value as 0/1), not like the parsed input format.
   If PART1? is non-nil, the input is filtered for values that lie within [-50..50] on each axis.
   If REVERSE-TIME? is non-nil, the order of the steps in the input is reversed."
  (let* ((input (input 22 source))
         (possibly-filtered-input (if part1?
                                      (22-part1-filter input)
                                      input))
         (lut (irregular-grid-lut possibly-filtered-input)))
    (funcall
     (if reverse-time? #'reverse #'identity)
     (loop for step in possibly-filtered-input
           collect (let ((min (if irregular-space?
                                  (xyz2irregular
                                   lut (getf step :xmin) (getf step :ymin) (getf step :zmin))
                                  (list (getf step :xmin) (getf step :ymin) (getf step :zmin))))
                         (max (if irregular-space?
                                  (xyz2irregular
                                   lut (1+ (getf step :xmax)) (1+ (getf step :ymax)) (1+ (getf step :zmax)))
                                  (list (getf step :xmax) (getf step :ymax) (getf step :zmax)))))
                     (if optimize?
                         (make-array (list 7)
                                     :element-type 'fixnum
                                     :initial-contents
                                     (list (first min) (first max)
                                           (second min) (second max)
                                           (third min) (third max)
                                           (if (getf step :set-value) 1 0)))
                         (list :set-value (getf step :set-value)
                               :xmin (first min) :xmax (first max)
                               :ymin (second min) :ymax (second max)
                               :zmin (third min) :zmax (third max))))))))
(22-input 'toy-example :part1? nil :irregular-space? T :optimize? nil :reverse-time? nil)
(22-input 'toy-example :irregular-space? nil)
(22-input 'toy-example :irregular-space? T)

#|
Decompose the space in an octree-oriented way, but use the observed endpoints as a performance-increasing guideline.
|#

(defun object-within-object? (xmin1 xmax1 ymin1 ymax1 zmin1 zmax1 xmin2 xmax2 ymin2 ymax2 zmin2 zmax2)
  "Whether object 1 shares all its cells with some or all of object 2.
   (Shared hyperplanes have no influence in any case.)"
  (declare (optimize speed)
           (fixnum xmin1 xmax1 ymin1 ymax1 zmin1 zmax1 xmin2 xmax2 ymin2 ymax2 zmin2 zmax2))
  ;;Exploiting the fact that objects contain at least 1 cell.
  (and (<= xmin2 xmin1 xmax1 xmax2)
       (<= ymin2 ymin1 ymax1 ymax2)
       (<= zmin2 zmin1 zmax1 zmax2)))

(defun object-intersects-object? (xmin1 xmax1 ymin1 ymax1 zmin1 zmax1 xmin2 xmax2 ymin2 ymax2 zmin2 zmax2)
  "Whether object 1 shares at least one cell with object 2."
  (declare (optimize speed)
           (fixnum xmin1 xmax1 ymin1 ymax1 zmin1 zmax1 xmin2 xmax2 ymin2 ymax2 zmin2 zmax2))
  ;;Exploiting the fact that objects contain at least 1 cell.
  (and (not (or (< xmax2 xmin1)
                (< xmax1 xmin2)))
       (not (or (< ymax2 ymin1)
                (< ymax1 ymin2)))
       (not (or (< zmax2 zmin1)
                (< zmax1 zmin2)))))

(defun block-contribution (&key lut xmin xnext ymin ynext zmin znext intersecting-steps)
  "Assume a default octree with branching blocks sorted in Morton / bit interleaving order (Z Y X).
   Now, alter the definition of the branching blocks: roughly divide the space of observed endpoints in two instead of using an uniform 2^height side length.
   Recursively call all branching blocks as long as the blocks intersect with at least one step in INTERSECTING-STEPS.
   As soon as a block is completely within all intersecting steps, take the latest set-value operation and return the volume in input space if it is T/1, 0 otherwise.
    Assumption: passed INTERSECTING-STEPS is already an time-reversed list.
   
   XMIN XNEXT YMIN YNEXT ZMIN ZNEXT define the current (irregularly shaped) block. The 'min' values are inclusive, the 'next' values exclusive.
   LUT is the lookup table which converts between the irregular space and the input space.
   INTERSECTING-STEPS is the set of currently intersecting steps.
   
   (This function is essentially the octree insertion, if adapted to the summing task of the puzzle.)
   (Also see #'submarine-explore of puzzle 12.)"
  (declare (optimize speed)
           (type fixnum xmin xnext ymin ynext zmin znext)
           (type (cons (simple-array fixnum (*))) lut))
  (if intersecting-steps
      (cond
        ;;Base case: the block is within all intersecting steps.
        ((loop for step in intersecting-steps
               always (locally (declare (type (simple-array fixnum (*)) step))
                        (object-within-object?
                         xmin (1- xnext) ymin (1- ynext) zmin (1- znext)
                         (aref step 0) (1- (aref step 1))
                         (aref step 2) (1- (aref step 3))
                         (aref step 4) (1- (aref step 5)))))
         ;;If activated, return the volume of the current block in input space.
         (if (= 1 (the (integer 0 1) (aref (locally (declare (type (cons (simple-array fixnum (*))) intersecting-steps))
                                             (car intersecting-steps))
                                           6)))
             (let* ((x-lut (the (simple-array fixnum (*)) (first lut)))
                    (y-lut (the (simple-array fixnum (*)) (second lut)))
                    (z-lut (the (simple-array fixnum (*)) (third lut))))
               (* (the (integer -262144 262144) (- (aref x-lut xnext) (aref x-lut xmin)))
                  (the (integer -262144 262144) (- (aref y-lut ynext) (aref y-lut ymin)))
                  (the (integer -262144 262144) (- (aref z-lut znext) (aref z-lut zmin)))))
             0))
        ;;Recursive case if there are intersecting steps: divide the space in the middle of the span of observed values (per axis).
        (T
         (let ((x-divisible? (< 1 (- xnext xmin)))
               (y-divisible? (< 1 (- ynext ymin)))
               (z-divisible? (< 1 (- znext zmin)))
               (sum 0)
               (xhalfsize (floor (- xnext xmin) 2))
               (yhalfsize (floor (- ynext ymin) 2))
               (zhalfsize (floor (- znext zmin) 2)))
           (locally (declare (type fixnum sum))
             ;;The branching block hyperrectangles are not of equal side length (even though the endpoints are strictly monotonically increasing when this function is called with irregular space) and may split along certain axes only (mainly because of unit-sized block sides).
             (loop for x? in (if x-divisible? '(nil T) '(nil))
                   do (loop for y? in (if y-divisible? '(nil T) '(nil))
                            do (loop for z? in (if z-divisible? '(nil T) '(nil))
                                     do (let ((new-xmin
                                                (if x?
                                                    (the fixnum (+ xmin xhalfsize))
                                                    xmin))
                                              (new-xnext
                                                (if (or x? (not x-divisible?))
                                                    xnext
                                                    (the fixnum (+ xmin xhalfsize))))
                                              (new-ymin
                                                (if y?
                                                    (the fixnum (+ ymin yhalfsize))
                                                    ymin))
                                              (new-ynext
                                                (if (or y? (not y-divisible?))
                                                    ynext
                                                    (the fixnum (+ ymin yhalfsize))))
                                              (new-zmin
                                                (if z?
                                                    (the fixnum (+ zmin zhalfsize))
                                                    zmin))
                                              (new-znext
                                                (if (or z? (not z-divisible?))
                                                    znext
                                                    (the fixnum (+ zmin zhalfsize)))))
                                          (incf sum
                                                (block-contribution
                                                 ;;The intersecting steps of any branching block are a subset of the intersecting steps of the current block.
                                                 :intersecting-steps
                                                 (loop for step in intersecting-steps
                                                       when (locally (declare (type (simple-array fixnum (*)) step))
                                                              (object-intersects-object?
                                                               new-xmin (1- new-xnext) new-ymin (1- new-ynext) new-zmin (1- new-znext)
                                                               (aref step 0) (1- (aref step 1))
                                                               (aref step 2) (1- (aref step 3))
                                                               (aref step 4) (1- (aref step 5))))
                                                         collect step)
                                                 :lut lut
                                                 :xmin new-xmin
                                                 :xnext new-xnext 
                                                 :ymin new-ymin
                                                 :ynext new-ynext
                                                 :zmin new-zmin
                                                 :znext new-znext))))))
             sum))))
      ;;No intersecting step.
      0))

(defun 22-solution (&key (source T) (part1? T))
  "Call #'block-contribution with the correct parameter values."
  (let* ((input           (22-input source :optimize? nil :irregular-space? nil :part1? part1? :reverse-time? T))
         (input-optimized (22-input source :optimize? T   :irregular-space? T   :part1? part1? :reverse-time? T))
         (lut (irregular-grid-lut input)))
    (block-contribution
     :intersecting-steps input-optimized
     :lut lut
     :xmin 0
     ;;The octree operations are done in irregular space (except the volume calculations, for which the lut is needed).
     :xnext (1- (array-total-size (first lut)))
     :ymin 0
     :ynext (1- (array-total-size (second lut)))
     :zmin 0
     :znext (1- (array-total-size (third lut))))))
;;Target: 39.
(22-solution :source 'toy-example :part1? T)
;;Target: 590784.
(22-solution :source 'middle-example :part1? T)

(defmethod solution ((day (eql 22)) (variant (eql 'part1)) source)
  "The number of activated reactor cells when reboot steps with interval endpoints in [-50,50] are considered."
  (22-solution :source source :part1? T))
(is 22 'part1 474140)

(defmethod solution ((day (eql 22)) (variant (eql 'part2)) source)
  "The number of activated reactor cells when all reboot steps are considered."
  (22-solution :source source :part1? nil))
(is 22 'part2 2758514936282235)

