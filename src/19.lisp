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

(defrule integer-z (and (? "-") integer)
  (:lambda (input)
    (let ((negativity-sign (elt input 0))
          (positive-number (elt input 1)))
      (parse-integer (format nil "~@[~A~]~A" negativity-sign positive-number)))))
(parse 'integer-z "123")
(parse 'integer-z "-456")

(defrule beacon-pos (and integer-z "," integer-z "," integer-z)
  (:lambda (input)
    ;;Actual X/Y/Z orientation is initially unknown, working with A/B/C.
    (let ((a (elt input 0))
          (b (elt input 2))
          (c (elt input 4)))
      (list a b c))))
(parse 'beacon-pos "-1,-2,1")

(defrule scanner-report (and "--- scanner " integer " ---" new
                             (+ (and beacon-pos (? new))))
  (:lambda (input)
    (let ((id (elt input 1))
          (beacons (elt input 4)))
      (list :id id :beacons (mapcar #'first beacons)))))
(parse 'scanner-report "--- scanner 0 ---
0,2,1
4,1,1
3,-3,1
")

(defrule 19-file (+ (and scanner-report (? new)))
  (:lambda (input)
    (let ((scanner-reports (make-hash-table)))
      (loop for report in (mapcar #'first input)
            do (setf (gethash (getf report :id) scanner-reports)
                     (getf report :beacons)))
      scanner-reports)))

(defmethod input ((day (eql 19)) source)
  "Hash table mapping scanner ids → scanner reports. Each scanner report consists of a list of beacons, a vector represented as a list with a b and c values."
  (parse '19-file (read-file day source)))
(input 19 T)
(input 19 'full)


;;;; Solution

#|
Spatial join with join condition "12 points which share the same relative positioning".

Naming conventions regarding the relative coordinate systems problem:
Any vector in relation to scanner 0, i.e. in relation to 0/0/0 with default rotation is in GLOBAL space.
 As #'grow-scanner-region at the end of this file shows, this must not be scanner 0, but typically is (depends on the input file actually).
Beacon vectors are typically in a rotated, LOCAL coordinate system. Local coordinate systems are numbered. Those numbers are called orientation-id.
|#

#|
Calculate vectors-to-other-beacons for each beacon.
Idea: all beacons in an overlapping area have the same distances towards each other. Identical beacons share identical distances.
 Think of the Voyager golden record cover image depicting the distances to our nearest pulsars, and how the distances remain the same, however this image is rotated.
The distances between the beacons within a scanner report are independent of any coordinate system rotation.
|#

(defun distances-between-beacons (beacons)
  "Determine the manhattan distance matrix for BEACONS."
  (loop for b in beacons
        collect (loop for other-b in beacons
                      collect
                      ;;The manhattan distance of the vectors seems good enough, no need for vectors.
                      (+ (abs (- (first b) (first other-b)))
                         (abs (- (second b) (second other-b)))
                         (abs (- (third b) (third other-b)))))))
(distances-between-beacons (gethash 0 (input 19 T)))
(equal (distances-between-beacons (getf (parse 'scanner-report "--- scanner 0 ---
-618,-824,-621
-537,-823,-458
-447,-329,318
404,-588,-901
544,-627,-890
528,-643,409
-661,-816,-575
390,-675,-793
423,-701,434
-345,-311,381
459,-707,401
-485,-357,347") :beacons))
       (distances-between-beacons (getf (parse 'scanner-report "--- scanner 1 ---
686,422,578
605,423,415
515,917,-361
-336,658,858
-476,619,847
-460,603,-452
729,430,532
-322,571,750
-355,545,-477
413,935,-424
-391,539,-444
553,889,-390") :beacons)))

#|
I made a detour during which I verified some assumptions about the input.
See 19-detour.lisp.
|#

;;Around 25 / 26 / 27 beacons per scanner.
(remove-duplicates (loop for beacons being the hash-values of (input 19 'full)
                         collect (length beacons)))

#|
Idea: find beacons by their position in the list / distance matrix.
|#

(defun raw-beacon-coordinates (beacons-left beacons-right)
  "Returns the twelve overlapping beacons and their vectors from the left and right scanner, respectively.
   NIL if the two scanner regions do not overlap."
  (let* ((distances-left (distances-between-beacons beacons-left))
         (distances-right (distances-between-beacons beacons-right)))
    ;;List with list as elements. Elements represent the beacon vector in the left/right scanner coordinate system, respectively.
    (loop with i = 0
          for outer in distances-left
          append (loop with j = 0
                       for inner in distances-right
                       unless (< (length (intersection inner outer)) 12)
                         collect (list (elt beacons-left i) (elt beacons-right j))
                       do (incf j))
          do (incf i))))
(raw-beacon-coordinates (gethash 0 (input 19 T))
                        (gethash 1 (input 19 T)))

(defun rotate-vector (vector orientation-id &optional reverse?)
  "Assuming A B C in a normalized coordinate system, rotate the vector such that it points towards the same direction in the target ORIENTATION-ID coordinate system.
   REVERSE? returns the original vector which was passed to a previous invocation of this function."
  (let ((a (first vector))
        (b (second vector))
        (c (third vector)))
    ;;(* 2 24): this function is used on raw beacon coordinates, for which the case of counter-rotated scanners must be considered.
    ;;I go for the full cartesian product in this place.
    (cond
      ;;abc
      ((= 0  orientation-id) (if reverse? (list (*  1 a) (*  1 b) (*  1 c)) (list (*  1 a) (*  1 b) (*  1 c))))
      ((= 1  orientation-id) (if reverse? (list (*  1 a) (*  1 b) (* -1 c)) (list (*  1 a) (*  1 b) (* -1 c))))
      ((= 2  orientation-id) (if reverse? (list (*  1 a) (* -1 b) (*  1 c)) (list (*  1 a) (* -1 b) (*  1 c))))
      ((= 3  orientation-id) (if reverse? (list (*  1 a) (* -1 b) (* -1 c)) (list (*  1 a) (* -1 b) (* -1 c))))
      ((= 4  orientation-id) (if reverse? (list (* -1 a) (*  1 b) (*  1 c)) (list (* -1 a) (*  1 b) (*  1 c))))
      ((= 5  orientation-id) (if reverse? (list (* -1 a) (*  1 b) (* -1 c)) (list (* -1 a) (*  1 b) (* -1 c))))
      ((= 6  orientation-id) (if reverse? (list (* -1 a) (* -1 b) (*  1 c)) (list (* -1 a) (* -1 b) (*  1 c))))
      ((= 7  orientation-id) (if reverse? (list (* -1 a) (* -1 b) (* -1 c)) (list (* -1 a) (* -1 b) (* -1 c))))
      ;;acb
      ((= 8  orientation-id) (if reverse? (list (*  1 a) (*  1 c) (*  1 b)) (list (*  1 a) (*  1 c) (*  1 b))))
      ((= 9  orientation-id) (if reverse? (list (*  1 a) (* -1 c) (*  1 b)) (list (*  1 a) (*  1 c) (* -1 b))))
      ((= 10 orientation-id) (if reverse? (list (*  1 a) (*  1 c) (* -1 b)) (list (*  1 a) (* -1 c) (*  1 b))))
      ((= 11 orientation-id) (if reverse? (list (*  1 a) (* -1 c) (* -1 b)) (list (*  1 a) (* -1 c) (* -1 b))))
      ((= 12 orientation-id) (if reverse? (list (* -1 a) (*  1 c) (*  1 b)) (list (* -1 a) (*  1 c) (*  1 b))))
      ((= 13 orientation-id) (if reverse? (list (* -1 a) (* -1 c) (*  1 b)) (list (* -1 a) (*  1 c) (* -1 b))))
      ((= 14 orientation-id) (if reverse? (list (* -1 a) (*  1 c) (* -1 b)) (list (* -1 a) (* -1 c) (*  1 b))))
      ((= 15 orientation-id) (if reverse? (list (* -1 a) (* -1 c) (* -1 b)) (list (* -1 a) (* -1 c) (* -1 b))))
      ;;bac
      ((= 16 orientation-id) (if reverse? (list (*  1 b) (*  1 a) (*  1 c)) (list (*  1 b) (*  1 a) (*  1 c))))
      ((= 17 orientation-id) (if reverse? (list (*  1 b) (*  1 a) (* -1 c)) (list (*  1 b) (*  1 a) (* -1 c))))
      ((= 18 orientation-id) (if reverse? (list (* -1 b) (*  1 a) (*  1 c)) (list (*  1 b) (* -1 a) (*  1 c))))
      ((= 19 orientation-id) (if reverse? (list (* -1 b) (*  1 a) (* -1 c)) (list (*  1 b) (* -1 a) (* -1 c))))
      ((= 20 orientation-id) (if reverse? (list (*  1 b) (* -1 a) (*  1 c)) (list (* -1 b) (*  1 a) (*  1 c))))
      ((= 21 orientation-id) (if reverse? (list (*  1 b) (* -1 a) (* -1 c)) (list (* -1 b) (*  1 a) (* -1 c))))
      ((= 22 orientation-id) (if reverse? (list (* -1 b) (* -1 a) (*  1 c)) (list (* -1 b) (* -1 a) (*  1 c))))
      ((= 23 orientation-id) (if reverse? (list (* -1 b) (* -1 a) (* -1 c)) (list (* -1 b) (* -1 a) (* -1 c))))
      ;;cab
      ((= 24 orientation-id) (if reverse? (list (*  1 b) (*  1 c) (*  1 a)) (list (*  1 c) (*  1 a) (*  1 b))))
      ((= 25 orientation-id) (if reverse? (list (*  1 b) (* -1 c) (*  1 a)) (list (*  1 c) (*  1 a) (* -1 b))))
      ((= 26 orientation-id) (if reverse? (list (* -1 b) (*  1 c) (*  1 a)) (list (*  1 c) (* -1 a) (*  1 b))))
      ((= 27 orientation-id) (if reverse? (list (* -1 b) (* -1 c) (*  1 a)) (list (*  1 c) (* -1 a) (* -1 b))))
      ((= 28 orientation-id) (if reverse? (list (*  1 b) (*  1 c) (* -1 a)) (list (* -1 c) (*  1 a) (*  1 b))))
      ((= 29 orientation-id) (if reverse? (list (*  1 b) (* -1 c) (* -1 a)) (list (* -1 c) (*  1 a) (* -1 b))))
      ((= 30 orientation-id) (if reverse? (list (* -1 b) (*  1 c) (* -1 a)) (list (* -1 c) (* -1 a) (*  1 b))))
      ((= 31 orientation-id) (if reverse? (list (* -1 b) (* -1 c) (* -1 a)) (list (* -1 c) (* -1 a) (* -1 b))))
      ;;bca
      ((= 32 orientation-id) (if reverse? (list (*  1 c) (*  1 a) (*  1 b)) (list (*  1 b) (*  1 c) (*  1 a))))
      ((= 33 orientation-id) (if reverse? (list (* -1 c) (*  1 a) (*  1 b)) (list (*  1 b) (*  1 c) (* -1 a))))
      ((= 34 orientation-id) (if reverse? (list (*  1 c) (*  1 a) (* -1 b)) (list (*  1 b) (* -1 c) (*  1 a))))
      ((= 35 orientation-id) (if reverse? (list (* -1 c) (*  1 a) (* -1 b)) (list (*  1 b) (* -1 c) (* -1 a))))
      ((= 36 orientation-id) (if reverse? (list (*  1 c) (* -1 a) (*  1 b)) (list (* -1 b) (*  1 c) (*  1 a))))
      ((= 37 orientation-id) (if reverse? (list (* -1 c) (* -1 a) (*  1 b)) (list (* -1 b) (*  1 c) (* -1 a))))
      ((= 38 orientation-id) (if reverse? (list (*  1 c) (* -1 a) (* -1 b)) (list (* -1 b) (* -1 c) (*  1 a))))
      ((= 39 orientation-id) (if reverse? (list (* -1 c) (* -1 a) (* -1 b)) (list (* -1 b) (* -1 c) (* -1 a))))
      ;;cba
      ((= 40 orientation-id) (if reverse? (list (*  1 c) (*  1 b) (*  1 a)) (list (*  1 c) (*  1 b) (*  1 a))))
      ((= 41 orientation-id) (if reverse? (list (* -1 c) (*  1 b) (*  1 a)) (list (*  1 c) (*  1 b) (* -1 a))))
      ((= 42 orientation-id) (if reverse? (list (*  1 c) (* -1 b) (*  1 a)) (list (*  1 c) (* -1 b) (*  1 a))))
      ((= 43 orientation-id) (if reverse? (list (* -1 c) (* -1 b) (*  1 a)) (list (*  1 c) (* -1 b) (* -1 a))))
      ((= 44 orientation-id) (if reverse? (list (*  1 c) (*  1 b) (* -1 a)) (list (* -1 c) (*  1 b) (*  1 a))))
      ((= 45 orientation-id) (if reverse? (list (* -1 c) (*  1 b) (* -1 a)) (list (* -1 c) (*  1 b) (* -1 a))))
      ((= 46 orientation-id) (if reverse? (list (*  1 c) (* -1 b) (* -1 a)) (list (* -1 c) (* -1 b) (*  1 a))))
      ((= 47 orientation-id) (if reverse? (list (* -1 c) (* -1 b) (* -1 a)) (list (* -1 c) (* -1 b) (* -1 a))))
      (T (error "Invalid ORIENTATION-ID.")))))
(loop for i from 0 below 48
      always (equal '(1 -2 3) (rotate-vector (rotate-vector '(1 -2 3) i) i T)))

(defun vector- (a b)
  "The difference between vector A and B."
  (list (- (first a) (first b))
        (- (second a) (second b))
        (- (third a) (third b))))

(defun bridge-scanners (beacon-from-left beacon-from-right)
  "Adds two vectors in order to determine a scanner position.
   BEACON-FROM-LEFT and BEACON-FROM-RIGHT both are a list that represents a vector towards a beacon, as seen from a scanner."
  ;;Trick is that the orientations will get rotated later on.
  (list (+ (first beacon-from-left) (first beacon-from-right))
        (+ (second beacon-from-left) (second beacon-from-right))
        (+ (third beacon-from-left) (third beacon-from-right))))

(defun joining-scanner-position-and-rotation (raw-beacon-coordinates &optional candidates)
  "Determine the vector to and the orientation of the right/joining scanner with the help of the vectors to overlapping beacons. Assumes the default orientation and position for the left scanner (0 and 0/0/0)."
  ;;Base idea: try to get to the same location (bridging from beacon to scanner) by searching in all possible directions (orientations). More and more beacons will hit the scanner position, while most other vectors won't be reached by multiple beacons. Stop as soon as there is only one position left (typically after the second/third beacon).
  (if (and raw-beacon-coordinates
           ;;#'cdr: not reduced to one possibility yet.
           (or (not candidates) (cdr candidates)))
      (joining-scanner-position-and-rotation
       (rest raw-beacon-coordinates)
       (let* ((left-value (first (car raw-beacon-coordinates)))
              (current-candidates
                (loop for i from 0 below 48
                      collect (list (bridge-scanners
                                     left-value
                                     (rotate-vector (second (car raw-beacon-coordinates)) i))
                                    i))))
         (if candidates
             ;;More and more beacon bridges reduce the problem.
             (intersection candidates current-candidates :test #'equal)
             ;;Initialization.
             current-candidates)))
      (car candidates)))
(joining-scanner-position-and-rotation
 (raw-beacon-coordinates (gethash 1 (input 19 T))
                         (gethash 3 (input 19 T))))

#|
From #'bidirectional-connections (19-detour.lisp). Plan: ((0 1) (1 3) (1 4) (4 2))
  0-1-3
    |
    4-2

0→0: ((0 0 0) 0)                            0 is at 0 0 0 rot 0
0→1: ((68 -1246 -43) 2)                     1 is at 68 -1246 -43 local rot 2 global rot 2
1→4: ((88 113 -1104) 36 local 28 global)    4 is at -20 -1133 1061 local rot 36 global rot 28
4→2: ((168 -1125 72) 22)                    2 is at 1105 -1205 1229 local rot 22
1→3: ((160 -1134 -23) 7)                    3 is at -92 -2380 -20 local rot 7
|#

(defun scanner-global-position (left-scanner-position left-scanner-rotation vector-to-right-scanner)
  "Determine the global position of the right scanner.
   VECTOR-TO-RIGHT-SCANNER is calculated by #'joining-scanner-position-and-rotation."
  (if (equal '(0 0 0) left-scanner-position)
      ;;Special case which is especially important for the first scanner.
      vector-to-right-scanner
      (rotate-vector (vector-
                      ;;Rotate the left scanner into its position when #'joining-scanner-position-and-rotation determined the vector...
                      (rotate-vector left-scanner-position left-scanner-rotation)
                      vector-to-right-scanner)
                     ;;... and rotate back to authoritative scanner 0.
                     left-scanner-rotation T)))
(joining-scanner-position-and-rotation
 (raw-beacon-coordinates (gethash 1 (input 19 T))
                         (gethash 3 (input 19 T))))
;;0→1 => (68 -1246 -43)
(scanner-global-position '(0 0 0) 0 '(68 -1246 -43))
;;1→4 => (-20 -1133 1061)
(scanner-global-position '(68 -1246 -43) 2 '(88 113 -1104))
;;4→2 => (1105 -1205 1229)
;;36 is relative to local coordinate system 2, 28 is global (see #'scanner-global-rotation).
(scanner-global-position '(-20 -1133 1061) 28 '(168 -1125 72))
;;1→3 => (-92 -2380 -20)
(scanner-global-position '(68 -1246 -43) 2 '(160 -1134 -23))

;;The key to this puzzle.
(defun scanner-global-rotation (scanner-globally first-beacon-globally first-beacon-locally)
  "Determines the global rotation of the scanner."
  ;;48: number of orientations.
  (loop for i from 0 below 48
        when (equal (vector- scanner-globally
                             first-beacon-globally)
                    (rotate-vector first-beacon-locally i T))
          return i))
(scanner-global-rotation '(-20 -1133 1061) '(-447 -329 318) '(-743 427 -804))
(scanner-global-rotation '(68 -1245 -43) '(404 -588 -901) '(-404 588 901))

#|
The circular dependency between #'scanner-global-position and #'scanner-global-rotation is resolved by scanner 0's known position and rotation.
|#

(defun beacons-global-position (left-scanner-position-global left-scanner-rotation-global left-beacons-local)
  "With the help of known left scanner position and rotation, rotate the shared beacons between two scanners into global space."
  (if (equal '(0 0 0) left-scanner-position-global)
      ;;Special case for the first scanner.
      left-beacons-local
      (loop for beacon in left-beacons-local
            collect (vector- left-scanner-position-global
                             (rotate-vector beacon left-scanner-rotation-global T)))))
(beacons-global-position '(0 0 0) 0 (gethash 0 (input 19 T)))
(beacons-global-position '(68 -1246 -43) 2 (gethash 1 (input 19 T)))

(defun join-scanners (input left-id left-pos left-rot right-id)
  "Known: (list LEFT-ID LEFT-POS LEFT-ROT).
   Wanted: (values right-pos right-rot).
   Returns NIL if LEFT-ID and RIGHT-ID do not overlap."
  (let ((raw-beacons (raw-beacon-coordinates (gethash left-id input)
                                             (gethash right-id input))))
    (when (and raw-beacons left-pos left-rot)
      ;;raw-beacons
      (let* ((vector-to-right
               (joining-scanner-position-and-rotation raw-beacons))
             (shared-beacons-global
               (beacons-global-position left-pos left-rot (mapcar #'first raw-beacons)))
             (right-scanner-position-global
               (scanner-global-position left-pos left-rot (first vector-to-right)))
             (right-scanner-rotation-global
               (scanner-global-rotation right-scanner-position-global
                                        (first shared-beacons-global)
                                        (first (mapcar #'second raw-beacons)))))
        (values right-scanner-position-global right-scanner-rotation-global)))))
;;0→1: Scanner 1 is at (68 -1246 -43) with global-rot 2.
(join-scanners (input 19 T) 0 '(0 0 0) 0 1)
;;1→4: Scanner 4 is at (-20 -1133 1061) with global-rot 28.
(join-scanners (input 19 T) 1 '(68 -1246 -43) 2 4)
;;4→2: Scanner 2 is at (1105 -1205 1229) with global-rot 11.
(join-scanners (input 19 T) 4 '(-20 -1133 1061) 28 2)
;;1→3: Scanner 3 is at (-92 -2380 -20) with global-rot 2.
(join-scanners (input 19 T) 1 '(68 -1246 -43) 2 3)
;;0 and 3 do not overlap.
(join-scanners (input 19 T) 0 '(0 0 0) 0 3)

(defun grow-scanner-region (input &key (last-scanner 0))
  "Starting with the first scanner region in *UNVISITED-IDS*, recursively attach other overlapping scanner regions.
   Expects context which is set up by #'19-solution.
   *BEACON-KNOWLEDGE* accumulates known beacons relative to the first scanner in *UNVISITED-IDS*. Maps (a b c) → T.
   *SCANNER-KNOWLEDGE* accumulates known scanner positions relative to the first scanner in *UNVISITED-IDS*. Maps id → (list position rotation)."
  (declare (special *beacon-knowledge* *scanner-knowledge* *unvisited-ids*))
  (when *unvisited-ids*
    (cond ((= 0 (hash-table-count *scanner-knowledge*))
           ;;Start.
           (loop for beacon in (gethash 0 input)
                 do (setf (gethash beacon *beacon-knowledge*) T))
           (setf (gethash (car *unvisited-ids*) *scanner-knowledge*) '((0 0 0) 0))
           (grow-scanner-region input :last-scanner 0))
          (T
           ;;Identify an overlapping scanner region.
           (loop for unvisited-id in *unvisited-ids*
                 do (multiple-value-bind (right-pos right-rot)
                        (join-scanners
                         input
                         last-scanner
                         (first (gethash last-scanner *scanner-knowledge*))
                         (second (gethash last-scanner *scanner-knowledge*))
                         unvisited-id)
                      (when (and right-pos right-rot)
                        (setf (gethash unvisited-id *scanner-knowledge*) (list right-pos right-rot))
                        (loop for beacon in (beacons-global-position
                                             right-pos
                                             right-rot
                                             (gethash unvisited-id input))
                              do (setf (gethash beacon *beacon-knowledge*) T))
                        (setf *unvisited-ids* (remove unvisited-id *unvisited-ids*))
                        (grow-scanner-region input :last-scanner unvisited-id))))))))

(defun 19-solution (input &optional part2?)
  "Set up a context for #'grow-scanner-region and collect informations about scanners and beacons."
  (let ((*beacon-knowledge* (make-hash-table :test 'equal))
        (*scanner-knowledge* (make-hash-table))
        (*unvisited-ids* (loop for k being the hash-keys of input
                               collect k)))
    (declare (special *beacon-knowledge* *scanner-knowledge* *unvisited-ids*))
    (grow-scanner-region input)
    (if part2?
        (loop for scanner-outer being the hash-values of *scanner-knowledge*
              maximize (loop for scanner-inner being the hash-values of *scanner-knowledge*
                             maximize
                             (let ((left (first scanner-outer))
                                   (right (first scanner-inner)))
                               (+ (abs (- (first left) (first right)))
                                  (abs (- (second left) (second right)))
                                  (abs (- (third left) (third right)))))))
        (hash-table-count *beacon-knowledge*))))
(19-solution (input 19 T))
(19-solution (input 19 T) T)

(defmethod solution ((day (eql 19)) (variant (eql 'part1)) source)
  "The number of beacons detected by all scanners."
  (19-solution (input day source)))
(is 19 'part1 79)

(defmethod solution ((day (eql 19)) (variant (eql 'part2)) source)
  "The largest City Block distance between all scanners."
  (19-solution (input day source) T))
(is 19 'part2 3621)

#|
Summary
=======

This puzzle had three "plot twists":
1. Identify a similarity in a freely rotatable space through coordinate-oblivious distances (#'distances-between-beacons).
2. The position in a cross join result determines the id of both columns (#'raw-beacon-coordinates).
3. The vector to and the rotation of the right scanner need to be found separately (#'scanner-global-position and #'scanner-global-rotation).
|#

