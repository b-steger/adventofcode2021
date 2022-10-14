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

(defmethod input ((day (eql 15)) source)
  "Multidimensional array representing a weighted graph with cost edges [0..9]. Easting/northing coordinate system from day 9 / day 11 is used."
  (let* ((nested-lists (parse '09-file (read-file day source)))
         (height (length nested-lists))
         (width (length (first nested-lists))))
    (make-array (list height width) :initial-contents nested-lists)))
(input 15 T)
(input 15 'full)

(defun input-15-part2 (input)
  "Input map, but repeated 5 times along each axis.
   Value is increased by floor(corrected-northing/height) + floor(easting/width) (wrapped by %9)."
  (let* ((ori-width (array-dimension input 1))
         (ori-height (array-dimension input 0))
         (part2-map (make-array (list (* 5 ori-height) (* 5 ori-width)))))
    (loop for northing from 0 below (array-dimension part2-map 1)
          do (loop for easting from 0 below (array-dimension part2-map 0)
                   do (setf (aref part2-map northing easting)
                            ;;1+: for the modulo calculation.
                            (1+ (rem (+ (aref input (mod northing ori-height) (mod easting ori-width))
                                        ;;4: zero-indexed maximal position in a tile.
                                        ;;#'- :northing axis is reversed.
                                        (- 4 (floor northing ori-height))
                                        (floor easting ori-width)
                                        ;;-1: for the modulo calculation.
                                        -1)
                                     9)))))
    part2-map))


;;;; Solution

#|
Shortest path with cost surface "risk" instead of "length" (a naming issue).
→ Adapting Dijkstra's algorithm from "Goodrich M. T., Tamassia R., Mount D. Data Structures & Algorithms in C++. John Wiley & Sons Inc., 2nd ed., 2011.", p. 639ff. (ALL simplifying assumptions are equivalent.)

I guess the intention of the part2 task is the optimization path from priority queues on unordered lists (p. 331) → queues on ordered lists (p. 332) → queues implemented with heaps (binary trees, p. 344) → Relaxed/Fibonacci heaps (p. 663) → pairing-min-heap (L. Fredman, R. Sedgewick, D. Sleator and R. Tarjan 'The Pairing Heap: A New Form of Self-Adjusting Heap', Algorithmica (1986) 1: 111--129.). Quicklisp package minheap happens to implement the last mentioned.

THINK: shortest path quadtree (Foundations of Multidimensional and Metric Data Structures. 2006, section 4.1.6 Nearest Neighbours in a Spatial Network, p. 508)?
|#

;;Puzzle 25 calls #'neighbour-lut.
(defun wrapping-move (map start by-vector)
  "Returns the new coordinates (as an rmindex) after adding BY-VECTOR to an rmindexed START.
   If the shift crosses the borders of the MAP, it will just wrap around, as if the MAP is tiled on an infinite plane."
  (let* ((width (array-dimension map 1))
         (height (array-dimension map 0))
         (new-easting (+ width
                         (easting map start)
                         (car by-vector)))
         (new-northing (+ height
                          (northing map start)
                          (cdr by-vector))))
    (en2rmindex map
                (rem new-easting width)
                (rem new-northing height))))
;;Target: (3 1).
(let* ((input (input 15 T))
       (index (wrapping-move (input 15 T) (en2rmindex (input 15 T) 2 3) (cons (+ (* 10 2) 1) (+ (* 9 4) 2)))))
  (list (easting input index) (northing input index)))

(defun neighbour-lut (map &key (lut *4direction-vectors*) positional-neighbours? wrapping-is-allowed?)
  "The neighbouring row-major indices for each rmindex in a two-dimensional MAP.
   Candidates for the directions are *4direction-vectors* (the four direct neighbours) and *8direction-vectors* (all 8 neighbours surrounding an index).
   Does not remove nil from the neighbour list when POSITIONAL-NEIGHBOURS?.
   Simulates an infinite map which is repeated in all directions if WRAPPING-IS-ALLOWED?. POSITIONAL-NEIGHBOURS? won't have any influence anymore in such a case.
   LUT means lookup-table."
  (let ((width (array-dimension map 1))
        (height (array-dimension map 0)))
    (make-array
     (array-dimensions map)
     :initial-contents
     (loop for northing from 0 below height
           collect
           (loop for easting from 0 below width
                 collect
                 (let ((neighbours
                         (loop for (dir . (vector)) in lut
                               collect
                               (funcall
                                (if wrapping-is-allowed? #'wrapping-move #'clip-move)
                                map
                                (array-row-major-index map northing easting)
                                (by-vector dir lut)))))
                   (if positional-neighbours?
                       neighbours
                       (remove nil neighbours))))))))
(neighbour-lut (input 15 T))

#+debug
(defmethod print-object ((obj pairing-heap::node) stream)
  (format stream "Preventing control stack exhaustion."))

(defun shortest-path (graph vertex-rmindex target-vertex-rmindex)
  "Dijkstra's algorithm on a weighted GRAPH.
   Return the sum of weights of the shortest path from VERTEX-RMINDEX (excluded) to TARGET-VERTEX-RMINDEX (included)."
  (let* ((width (array-dimension graph 1))
         (height (array-dimension graph 0))
         ;;d-label: array storing labels for a vertex - the length of a shortest path from VERTEX-RMINDEX to the respective entry.
         (d-label (make-array (array-dimensions graph)
                              ;;∞ in (unoptimized) CL??? → An impossible result cost, which is more than the longest path (repeated Zs) with more than a vertex can contribute.
                              :initial-element (* 10 width height)))
         (queue (make-instance 'pairing-heap:pairing-heap))
         (neighbour-lut (neighbour-lut graph)))
    
    ;;Set the start by being the next cheapest node (book calls this "bootstrapping trick").
    (setf (row-major-aref d-label vertex-rmindex) 0)
    
    ;;Initialize the queue.
    ;;The queue's priority is the edge weight, the queue's returned value is the rmindex.
    (let* ((nodes
             (make-array
              (list (array-total-size graph))
              :initial-contents
              (loop for i from 0 below (array-total-size graph)
                    collect (pairing-heap:insert queue (row-major-aref d-label i) i))))
           ;;Avoids circular paths.
           (visited (make-array (list (array-total-size graph)) :initial-element nil)))
      
      (loop
        with u = nil
        while (not (pairing-heap:empty-p queue))
        do (setf u (pairing-heap:extract-min queue)
                 (aref visited u) T)
        do (loop
             for z in (row-major-aref neighbour-lut u)
             unless (aref visited z)
               ;;Edge relaxation.
               do (let ((new-estimate (+ (row-major-aref d-label u)
                                         (row-major-aref graph z))))
                    (when (< new-estimate (row-major-aref d-label z))
                      (setf (row-major-aref d-label z) new-estimate)
                      ;;UPDATE graph SET label=new_estimate WHERE label=z; for indexed column graph(label).
                      (pairing-heap:decrease-key queue (aref nodes z) new-estimate)))))
      (row-major-aref d-label target-vertex-rmindex))))

(defun solution-15 (source part2?)
  "The shortest path cost for the part1/part2 map."
  (let ((input (if part2?
                   (input-15-part2 (input 15 source))
                   (input 15 source))))
    (shortest-path
     input
     ;;The row-major-index (rmindex) of the top left vertex (the start).
     (array-row-major-index input (1- (array-dimension input 0)) 0)
     ;;The row-major-index (rmindex) of the bottom right vertex (the end).
     (array-row-major-index input 0 (1- (array-dimension input 1))))))

(defmethod solution ((day (eql 15)) (variant (eql 'part1)) source)
  "Shortest path cost with cost surface \"risk\" (unmodified map)."
  (solution-15 source nil))
(is 15 'part1 40)

(defmethod solution ((day (eql 15)) (variant (eql 'part2)) source)
  "Shortest path cost with cost surface \"risk\" (enlarged map)."
  (solution-15 source T))
(is 15 'part2 315)

