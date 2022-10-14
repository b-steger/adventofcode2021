;;;; Advent of code 2021 solutions
;;;; Copyright (C) 2022 Benedikt Steger <b.steger@protonmail.ch>
;;;; 
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Affero General Public License along with this program. If not, see <http://www.gnu.org/licenses/>.

(in-package :adventofcode2021)

#|
Historical but working code which contributed to the development of the solution for the puzzle 19.
The most interesting form is the definition of #'scanner-join-plan.
|#

(defun scanner-overlap? (scanner-report1 scanner-report2)
  "NIL if the two scanner reports do not overlap.
   (Otherwise, the list of intra-beacon distances for each listed beacon - hardly useful.)"
  (let ((beacons1 (distances-between-beacons scanner-report1))
        (beacons2 (distances-between-beacons scanner-report2)))
    (remove-if (lambda (x)
                 ;;Filter out distances to itself and random coincidences.
                 (< (length x) 10))
               (loop for outer in beacons1
                     append (loop for inner in beacons2
                                  collect (intersection inner outer))))))
(scanner-overlap? (gethash 0 (input 19 T)) (gethash 2 (input 19 T)))
(scanner-overlap? (gethash 0 (input 19 T)) (gethash 1 (input 19 T)))
(scanner-overlap? (gethash 1 (input 19 T)) (gethash 3 (input 19 T)))
(scanner-overlap? (gethash 2 (input 19 T)) (gethash 4 (input 19 T)))
(scanner-overlap? (gethash 0 (input 19 'full)) (gethash 7 (input 19 'full)))

#|
Next: determine the join tree.
|#

(defun bidirectional-connections (input)
  "Computes the bidirectional connections between scanner regions."
  (loop for i from 0 below (hash-table-count input)
        append (loop for j from 0 below (hash-table-count input)
                      when (and (/= i j)
                                (scanner-overlap? (gethash i input) (gethash j input)))
                        collect (list i j))))
#|=> ((0 1) (1 0) (1 3) (1 4) (2 4) (3 1) (4 1) (4 2))
  => 0-1-3
       |
       4-2|#
(bidirectional-connections (input 19 T))
#|(bidirectional-connections (input 19 'full))
;;All regions are connected:
;;=> (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32)
(remove-duplicates (sort (loop for item in (bidirectional-connections (input 19 'full))
                               append item)
                         #'<))|#

(defun scanner-join-plan (scanner-connections unvisited-ids)
  "Topologically sort UNVISITED-IDS such that a region can be grown by (ad-)joining scanner regions.
   Returns a list of scanner region pairs that need to join in order grow the existing, growing map.
   Each entry lists the scanner id belonging to the growing result region first."
  (when unvisited-ids
    (let ((next
            ;;The first pair with an unvisited id which does not link only unvisited ids.
            (loop for pair in scanner-connections
                  when (and (not (and (member (first pair) unvisited-ids)
                                      (member (second pair) unvisited-ids)))
                            (or (member (first pair) unvisited-ids)
                                (member (second pair) unvisited-ids)))
                    ;;The first entry should reference a region which belongs to the growing region.
                    return (if (member (first pair) unvisited-ids)
                               (reverse pair)
                               pair))))
      (if next
          (cons next
                (scanner-join-plan
                 scanner-connections
                 ;;Sort of (- unvisited-ids next) or (remove next unvisited-ids).
                 (set-difference unvisited-ids next)))
          ;;Bootstrapping since all IDs are unvisited.
          ;;Instead of selecting the first pair which contains the bootstrapped scanner id, the recursive call is made instantly since the next one will always select the pair in question first.
          (scanner-join-plan scanner-connections (rest unvisited-ids))))))
;;=> ((0 1) (1 3) (1 4) (4 2))
(scanner-join-plan (bidirectional-connections (input 19 T))
                   (loop for k being the hash-keys of (input 19 T)
                         collect k))

