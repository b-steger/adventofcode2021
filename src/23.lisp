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

(defrule amphipod (or "A" "B" "C" "D")
  (:lambda (input)
    (cond ((string= "A" input) 'a)
          ((string= "B" input) 'b)
          ((string= "C" input) 'c)
          ((string= "D" input) 'd))))
(parse 'amphipod "A")

(defrule ignorable-painting (* (or " " "#" "." new)))

(defrule room-slice (and ignorable-painting amphipod
                         ignorable-painting amphipod
                         ignorable-painting amphipod
                         ignorable-painting amphipod
                         ignorable-painting)
  (:lambda (input)
    (let ((rooma (elt input 1))
          (roomb (elt input 3))
          (roomc (elt input 5))
          (roomd (elt input 7)))
      (list rooma roomb roomc roomd))))

(defrule burrow (+ room-slice)
  (:lambda (input)
    (list (mapcar #'first input)
          (mapcar #'second input)
          (mapcar #'third input)
          (mapcar #'fourth input))))

(defmethod input ((day (eql 23)) source)
  "A burrow listing sorted side rooms with unorganized amphipods (symbols 'A 'B 'C 'D). The side rooms are lists which behave like stacks, i.e. the first entry is the nearest to the hallway."
  (parse 'burrow (read-file 23 source)))
(input 23 'example)


;;;; Solution

#|
Reminds of day 7, but sounds more like a job for Prolog this time.
 → Screamer.
(Edit: I stumbled over the solution while I prepared the rule explanations - I did not follow the Screamer path further. Still, I use Screamer in #'simple-path and #'shortest-graph-path.)
|#

#|
World representation:

A---B---C---D---E---F---G  hallway (edge weights 1 2 2 2 2 1)
     \ / \ / \ / \ /               (edge weights 2 2 2 2 2 2 2 2)
      H   L   P   U        level 0 (T is not allowed in ordinary lambda lists)
      |   |   |   |                (edge weights 1 1 1 1)
      I   M   Q   V        level 1 (ignored in part1)
      |   |   |   |                (edge weights 1 1 1 1)
      J   N   R   W        level 2 (ignored in part1)
      |   |   |   |                (edge weights 1 1 1 1)
      K   O   S   X        level 3

As an array:
0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22
A B C D E F G H I J  K  L  M  N  O  P  Q  R  S  U  V  W  X
Input positions: part1: ((7 10) (11 14) (15 18) (19 22))
                 part2: ((7 8 9 10) (11 12 13 14) (15 16 17 18) (19 20 21 22))

Animal type mapping: A→1 B→2 C→3 D→4. (0 is used for the empty space in the world array.)
 Only the animal type is recorded, not the identity.
|#

(defparameter *animal-map* '(reserved a b c d)
  "The map of animals for puzzle 23.")

(defparameter *node-map* '(a b c d e f g h i j k l m n o p q r s u v w x)
  "The nodes of the world graph of puzzle 23.")

(defun edges (part2?)
  "All edges of the world of puzzle 23 with the PART2? modification if wished."
  (append
   ;;The edges of the hallway.
   '((a b 1) (b c 2) (c d 2) (d e 2) (e f 2) (f g 1)
     (b h 2) (h c 2)
     (c l 2) (l d 2)
     (d p 2) (p e 2)
     (e u 2) (u f 2))
   ;;The edges of the side rooms.
   (if part2?
       '((h i 1) (i j 1) (j k 1)
         (l m 1) (m n 1) (n o 1)
         (p q 1) (q r 1) (r s 1)
         (u v 1) (v w 1) (w x 1))
       '((h k 1) (l o 1) (p s 1) (u x 1)))))

(defun 23-input (source &key part2? array?)
  "Input of day 23 with customizations.
   If PART2? is non-nil, enlarge the world and add the extra animals as required by the spec.
   If ARRAY? is non-nil, return the initial world state as an array."
  (let* ((animals-part1 (input 23 source))
         (animals-in-rooms
           (if (or part2? array?)
               (mapcar (lambda (x a b)
                         (list (first x) a b (second x)))
                       animals-part1
                       (if part2? '(D C B A) '(nil nil nil nil))
                       (if part2? '(D B A C) '(nil nil nil nil)))
               animals-part1))
         (animals-mapped-to-numbers
           (loop for room in animals-in-rooms
                 collect (mapcar (lambda (x)
                                   (case x
                                     (a 1) (b 2) (c 3) (d 4) (T 0)))
                                 room))))
    (if array?
        (make-array (length *node-map*)
                    :element-type 'fixnum
                    :initial-contents (apply #'concatenate 'cons
                                             (cons '(0 0 0 0 0 0 0)
                                                   animals-mapped-to-numbers)))
        animals-in-rooms)))
(23-input 'example :part2? nil :array? T)
(23-input 'example :part2? T :array? T)

(defun quick-world (a b c d e f g h i j k l m n o p q r s u v w x)
  "Quickly represent world states (and get convenient argument highlights by the editor)."
  (make-array (length *node-map*)
              :element-type 'fixnum
              :initial-contents (list a b c d e f g h i j k l m n o p q r s u v w x)))

(defun world-with-room-configuration (&key animal-type (level0 0) (level1 0) (level2 0) (level3 0))
  "Return a world where the room for ANIMAl-TYPE is filled with LEVEL0 LEVEL1 LEVEL2 LEVEL3."
  (cond ((= 1 animal-type) (quick-world 0 0 0 0 0 0 0 level0 level1 level2 level3 0 0 0 0 0 0 0 0 0 0 0 0))
        ((= 2 animal-type) (quick-world 0 0 0 0 0 0 0 0 0 0 0 level0 level1 level2 level3 0 0 0 0 0 0 0 0))
        ((= 3 animal-type) (quick-world 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 level0 level1 level2 level3 0 0 0 0))
        ((= 4 animal-type) (quick-world 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 level0 level1 level2 level3))
        (T (error "Wrong animal type."))))

(defun diashow (worlds)
  "Render each world in WORLDS in the debugger."
  (when worlds
    (break "~A"
           ;;Prevents #n= and #n# syntax (^= manually shadow *print-circle* T).
           (apply #'format nil "~%#############
#~A~A ~A ~A ~A ~A~A#
###~A ~A ~A ~A###
  #~A ~A ~A ~A#
  #~A ~A ~A ~A#
  #~A ~A ~A ~A#
  #########" (let ((rotate-rooms (list 0 1 2 3 4 5 6 7 11 15 19 8 12 16 20 9 13 17 21 10 14 18 22)))
               (mapcar (lambda (x)
                         (let ((value (aref (car worlds) x)))
                           (if (= 0 value) " " value)))
                       rotate-rooms))))
    (diashow (rest worlds))))
#|(diashow (list (quick-world 4 3 2 1 2 3 4 1 2 3 4 5 6 7 8 9 8 7 6 5 4 3 2)
               (quick-world 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)))|#

#|
World animation: routing.
|#

(defun neighbouring-nodes (edges node-name)
  "The neighbouring nodes of NODE-NAME.
   Also see #'neighbours-of-node."
  (loop for edge in edges
        when (eql node-name (first edge))
          collect (second edge)
        when (eql node-name (second edge))
          collect (first edge)))
(neighbouring-nodes (edges T) 'l)

(defclass graph-node ()
  ((name :initform nil :initarg :name :accessor name)
   (visited? :initform nil :initarg :visited? :accessor visited?)
   (neighbours :initform nil :initarg :neighbours :accessor neighbours)))

(defun 23-world (part2?)
  "A hash table containing instances of graph-node."
  (let ((edges (edges part2?))
        (world (make-hash-table)))
    (loop for node in *node-map*
          do (setf (gethash node world)
                   (make-instance 'graph-node
                                  :name node
                                  :neighbours (neighbouring-nodes edges node))))
    world))

(screamer::defun simple-path (world from to)
  "Non-deterministic function which finds a path whose node list is unique.
   (Adapted from the example 3 of the Screamer manual / screams.lisp of the Quicklisp package.)"
  (when (visited? (gethash from world))
    (screamer:fail))
  ;;This will be undone upon backtracking.
  (screamer:local (setf (visited? (gethash from world)) T))
  (screamer:either
    (progn (unless (eq from to) (screamer:fail))
           (list from))
    (cons from (simple-path world
                            (screamer:a-member-of (neighbours (gethash from world)))
                            to))))
(screamer:all-values (simple-path (23-world T) 'a 'c))

(defun shortest-graph-path (world edges from to &key array-index-representation?)
  "Shortest path between the two nodes FROM and TO in WORLD graph (use #'23-world), applying the cost information from EDGES (use #'edges).
   Uses a non-deterministic approach internally.
   ARRAY-REPRESENTATION? returns world indices instead of letters/symbols and removes the initial starting node."
  (flet ((path-cost (path)
           "The sum of the edge costs of PATH."
           (loop for (a . d) on path
                 when d
                   sum (third (find-if (lambda (x)
                                         (or (and (equal a (first x))
                                                  (equal (car d) (second x)))
                                             (and (equal a (second x))
                                                  (equal (car d) (first x)))))
                                       edges)))))
    (let* ((paths (screamer:all-values (simple-path world from to)))
           (shortest-path (first (sort paths (lambda (x y) (< (path-cost x) (path-cost y)))))))
      (values (if array-index-representation?
                  (when shortest-path
                    (make-array (1- (length shortest-path))
                                :element-type 'fixnum
                                :initial-contents (rest (mapcar (lambda (x) (position x *node-map*)) shortest-path))))
                  shortest-path)
              (path-cost shortest-path)))))
(shortest-graph-path (23-world T) (edges T) 'a 'x :array-index-representation? nil)

(defun routing-lut (world edges &key array-index-representation?)
  "A lookup table which contains the cross join of all nodes (a list with the shortest path and its cost).
   Realized as a two-dimensional array with the mapping from *node-map*.
   WORLD and EDGES must match the wished puzzle part. As *node-map* is used for both puzzle parts, part 1 is free to ignore unused indices.
   ARRAY-INDEX-REPRESENTATION? is passed to #'shortest-graph-path unchanged."
  (make-array (list (length *node-map*)
                    (length *node-map*))
              :initial-contents
              (loop for from in *node-map*
                    collect (loop for to in *node-map*
                                  collect (multiple-value-list
                                           (shortest-graph-path world edges from to
                                                                :array-index-representation? array-index-representation?))))))
(routing-lut (23-world nil) (edges nil) :array-index-representation? T)

#|
World rules.
|#

(defun animals-sorted? (world part2?)
  "Whether all animals are in their respective room (termination condition)."
  (and
   ;;Empty hallway (cl:and is a macro → fast failure).
   (= 0 (aref world 0))
   (= 0 (aref world 1))
   (= 0 (aref world 2))
   (= 0 (aref world 3))
   (= 0 (aref world 4))
   (= 0 (aref world 5))
   (= 0 (aref world 6))
   ;;Each room (written on one line each) has only its respective animal type.
   (= 1 (aref world 7)) (= 1 (aref world 10))
   (= 2 (aref world 11)) (= 2 (aref world 14))
   (= 3 (aref world 15)) (= 3 (aref world 18))
   (= 4 (aref world 19)) (= 4 (aref world 22))
   (if part2?
       (and (= 1 (aref world 8)) (= 1 (aref world 9))
            (= 2 (aref world 12)) (= 2 (aref world 13))
            (= 3 (aref world 16)) (= 3 (aref world 17))
            (= 4 (aref world 20)) (= 4 (aref world 21)))
       T)))
(animals-sorted? (quick-world 0 0 0 0 0 0 0 1 0 0 1 2 0 0 2 3 0 0 3 4 0 0 4) nil)
(animals-sorted? (quick-world 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4) T)

(defun free-room-node (&key animal-type world part2? (level 0) last-empty-node)
  "Helper for moving into a room.
   Return the next free room node (as index) or nil. The non-full room must be empty or only inhabitated by animals of the same ANIMAL-TYPE.
   Assumption: all other animals in the room have moved to level 3 if possible. (Given since this function itself is used for movements of others.)
   LEVEL: room nodes nearest to the hallway are level 0. Part 1 will jump from level 0 to 3.
   Only ANIMAL-TYPE WORLD PART2? are part of the public interface."
  ;;7: skip the hallway; 4: size of room; 1-: first room node is zero-indexed
  (let* ((new-node (+ 7 level (* 4 (1- animal-type))))
         (new-empty? (= 0 (aref world new-node)))
         (new-empty-node (if new-empty? new-node last-empty-node))
         (correct-animal-type? (= animal-type (aref world new-node))))
    (when (or new-empty? correct-animal-type?)
      ;;3: The node farthest from the hallway is at level 3, irrespective of part1/part2.
      (if (< level 3)
          (free-room-node
           :animal-type animal-type
           :world world
           :part2? part2?
           ;;3: there is only one recursive call for part1.
           :level (if part2? (1+ level) 3)
           :last-empty-node new-empty-node)
          new-empty-node))))
(free-room-node :animal-type 1 :world (quick-world 0 0 0 0 0 0 0 0 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4) :part2? T)
(free-room-node :animal-type 4 :world (quick-world 0 0 0 0 0 0 0 0 1 1 2 2 2 2 2 0 0 0 0 0 0 1 2) :part2? T)

(defun outgoing-animal-node (&key animal-type world part2? (level 0) first-nonempty-node found-wrong-in-room?)
  "Helper for moving out of a room.
   Returns the node in an ANIMAL-TYPE-side-room of the animal nearest to the hallway.
   NIL if the room is empty, or if the existing animals are all of the same, correct type in the room in question.
   Only ANIMAL-TYPE WORLD PART2? are part of the public interface."
  ;;Magic numbers see #'free-room-node.
  (let* ((new-node (+ 7 level (* 4 (1- animal-type))))
         (matching-animal-type? (= animal-type (aref world new-node)))
         (new-empty? (= 0 (aref world new-node)))
         (new-first-nonempty-node (or first-nonempty-node (unless new-empty? new-node)))
         (new-found-wrong-in-room? (or found-wrong-in-room? (and (not new-empty?) (not matching-animal-type?)))))
    (cond ((< level 3)
           (outgoing-animal-node
            :animal-type animal-type
            :world world
            :part2? part2?
            :level (if part2? (1+ level) 3)
            :first-nonempty-node new-first-nonempty-node
            :found-wrong-in-room? new-found-wrong-in-room?))
          (new-found-wrong-in-room?
           new-first-nonempty-node))))
(outgoing-animal-node :animal-type 4 :world (quick-world 0 0 0 0 0 0 0 0 1 1 1 2 2 2 2 0 0 0 0 0 4 4 3) :part2? T)

(defun room-movement-lut (&key part2? in?)
  "Calculate all performed calls to #'free-room-node (if IN? is non-nil) or to #'outgoing-animal-node (if IN? is nil).
   Returns a multidimensional array with the dimensions animal-type level0 [level1 level2] level3.
   (Actually calculates illogical calls, too. This is not a problem; see the discussion in the docstring of #'free-room-node.)"
  (let ((animal-types '(0 1 2 3 4))
        (rooms '(1 2 3 4)))
    (make-array
     (if part2?
         (list (length rooms) (length animal-types) (length animal-types) (length animal-types) (length animal-types))
         (list (length rooms) (length animal-types) (length animal-types)))
     :element-type '(or null fixnum)
     :initial-contents
     (if part2?
         (loop for room in rooms
               collect
               (loop for level0 in animal-types
                     collect
                     (loop for level1 in animal-types
                           collect
                           (loop for level2 in animal-types
                                 collect
                                 (loop for level3 in animal-types
                                       collect
                                       (funcall (if in? #'free-room-node #'outgoing-animal-node)
                                                :animal-type room
                                                :part2? part2?
                                                :world (world-with-room-configuration
                                                        :animal-type room
                                                        :level0 level0 :level1 level1
                                                        :level2 level2 :level3 level3)))))))
         (loop for room in rooms
               collect
               (loop for level0 in animal-types
                     collect
                     (loop for level3 in animal-types
                           collect
                           (funcall (if in? #'free-room-node #'outgoing-animal-node)
                                    :animal-type room
                                    :part2? part2?
                                    :world (world-with-room-configuration :animal-type room :level0 level0 :level3 level3)))))))))
(room-movement-lut :in? T :part2? T)

(defun can-move? (&key world from to routing-lut)
  "Whether the animal at node FROM can move unhindered to node TO in WORLD."
  (declare (optimize speed))
  (every (lambda (x) (= 0 (aref (the (simple-array fixnum *) world) x)))
         (the (simple-array fixnum *)
              (first (aref (the (simple-array T (23 23))
                                routing-lut)
                           from to)))))
(can-move? :world (quick-world 0 4 0 0 0 1 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0)
           :from 1 :to 22
           :routing-lut (routing-lut (23-world T) (edges T) :array-index-representation? T))

(defun valid-moves (&key world part2? routing-lut in-lut out-lut)
  "Return possible moves in a WORLD. Result format: list of (list start end).
   This function returns a single move if an animal can reach its room unhindered. This is ok since the animals must cover their distances one day anyway.
    → Choice points are only introduced when there are real decisions."
  ;;THINK: possibly optimize the solution by sorting the return value by cost.
  (declare (optimize speed)
           (type (simple-array fixnum *) world))
  ;;1. Get the next free (and legal) nodes per room for hallway animals.
  (let* ((room1-node (if part2?
                         (locally (declare (type (simple-array (or null fixnum) (4 5 5 5 5)) in-lut))
                           (aref in-lut 0 (aref world 7) (aref world 8) (aref world 9) (aref world 10)))
                         (locally (declare (type (simple-array (or null fixnum) (4 5 5)) in-lut))
                           (aref in-lut 0 (aref world 7) (aref world 10)))))
         (room2-node (if part2?
                         (locally (declare (type (simple-array (or null fixnum) (4 5 5 5 5)) in-lut))
                           (aref in-lut 1 (aref world 11) (aref world 12) (aref world 13) (aref world 14)))
                         (locally (declare (type (simple-array (or null fixnum) (4 5 5)) in-lut))
                           (aref in-lut 1 (aref world 11) (aref world 14)))))
         (room3-node (if part2?
                         (locally (declare (type (simple-array (or null fixnum) (4 5 5 5 5)) in-lut))
                           (aref in-lut 2 (aref world 15) (aref world 16) (aref world 17) (aref world 18)))
                         (locally (declare (type (simple-array (or null fixnum) (4 5 5)) in-lut))
                           (aref in-lut 2 (aref world 15) (aref world 18)))))
         (room4-node (if part2?
                         (locally (declare (type (simple-array (or null fixnum) (4 5 5 5 5)) in-lut))
                           (aref in-lut 3 (aref world 19) (aref world 20) (aref world 21) (aref world 22)))
                         (locally (declare (type (simple-array (or null fixnum) (4 5 5)) in-lut))
                           (aref in-lut 3 (aref world 19) (aref world 22))))))
    ;;2. All animals in the hallway should check whether the path is free to those next free nodes. If there is such a possibility, return it.
    (or (loop for i from 0 below 7
              do (let* ((animal (aref world i))
                        (to (cond ((= 1 animal) room1-node)
                                  ((= 2 animal) room2-node)
                                  ((= 3 animal) room3-node)
                                  ((= 4 animal) room4-node)
                                  (T 0))))
                   (when (and (< 0 animal)
                              to
                              (can-move? :world world
                                         :from i
                                         :to to
                                         :routing-lut routing-lut))
                     ;;Move instantly.
                     (return (list (list i to))))))
        ;;3. The animals nearest to the hallway of each room must move out to the hallway. Collect all free paths.
        ;;THINK: optimize with heuristics? → Certain types do not have to move to A or G etc. → True?
        (let* ((room1-launch-node (if part2?
                                      (locally (declare (type (simple-array (or null fixnum) (4 5 5 5 5)) out-lut))
                                        (aref out-lut 0 (aref world 7) (aref world 8) (aref world 9) (aref world 10)))
                                      (locally (declare (type (simple-array (or null fixnum) (4 5 5)) out-lut))
                                        (aref out-lut 0 (aref world 7) (aref world 10)))))
               (room2-launch-node (if part2?
                                      (locally (declare (type (simple-array (or null fixnum) (4 5 5 5 5)) out-lut))
                                        (aref out-lut 1 (aref world 11) (aref world 12) (aref world 13) (aref world 14)))
                                      (locally (declare (type (simple-array (or null fixnum) (4 5 5)) out-lut))
                                        (aref out-lut 1 (aref world 11) (aref world 14)))))
               (room3-launch-node (if part2?
                                      (locally (declare (type (simple-array (or null fixnum) (4 5 5 5 5)) out-lut))
                                        (aref out-lut 2 (aref world 15) (aref world 16) (aref world 17) (aref world 18)))
                                      (locally (declare (type (simple-array (or null fixnum) (4 5 5)) out-lut))
                                        (aref out-lut 2 (aref world 15) (aref world 18)))))
               (room4-launch-node (if part2?
                                      (locally (declare (type (simple-array (or null fixnum) (4 5 5 5 5)) out-lut))
                                        (aref out-lut 3 (aref world 19) (aref world 20) (aref world 21) (aref world 22)))
                                      (locally (declare (type (simple-array (or null fixnum) (4 5 5)) out-lut))
                                        (aref out-lut 3 (aref world 19) (aref world 22))))))
          (loop for node in (list room1-launch-node room2-launch-node room3-launch-node room4-launch-node)
                when node
                  append (loop for i from 0 below 7
                               when (can-move? :world world :from node :to i :routing-lut routing-lut)
                                 collect (list node i)))))))
(let ((routing-lut (routing-lut (23-world T) (edges T) :array-index-representation? T))
      (in-lut (room-movement-lut :in? T :part2? T))
      (out-lut (room-movement-lut :in? nil :part2? T)))
  (loop for world in (list (quick-world 0 1 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0)
                           (23-input T :part2? T :array? T)
                           ;;This is the example, but the animal at 11 (L) has moved to 3 (D).
                           (quick-world 0 0 0 3 0 0 0 2 4 4 1 0 3 2 4 2 2 1 3 4 1 3 1)
                           (quick-world 2 0 0 0 0 0 0 0 0 0 1 3 0 0 4 2 0 0 3 4 0 0 1)
                           (quick-world 2 1 0 0 0 0 0 0 0 0 0 3 0 0 4 2 0 0 3 4 0 0 1))
        collect (valid-moves :world world :part2? T :routing-lut routing-lut :in-lut in-lut :out-lut out-lut)))

(defun move-animal (&key world from to destructively?)
  "Move animal at FROM to TO without any checks."
  (let ((result-world (if destructively? world (copy-seq world))))
    (setf (aref result-world to) (aref result-world from)
          (aref result-world from) 0)
    result-world))
(let ((world (23-input T :part2? T :array? T)))
  (move-animal :world world :from 7 :to 1 :destructively? T)
  world)

(defun organize-animals (&key routing-lut in-lut out-lut part2? (running-score 0))
  "Try out each valid move and recursively continue until all animals are sorted.
   Prunes the search tree by stopping as soon as the internal RUNNING-SCORE is higher than the score of a previously found solution.
   Needs additional context such as the one provided by #'23-solution."
  (declare (special *minimal-score* *world*))
  ;;(diashow (list *world*))
  (when (< running-score *minimal-score*)
    (if (animals-sorted? *world* part2?)
        ;;The new score is lower than *minimal-score*, i.e. a better solution is found.
        (setf *minimal-score* running-score)
        (loop for move in (valid-moves :world *world* :part2? part2? :routing-lut routing-lut :in-lut in-lut :out-lut out-lut)
              do (progn
                   (move-animal :world *world* :from (first move) :to (second move) :destructively? T)
                   (organize-animals :routing-lut routing-lut
                                     :in-lut in-lut
                                     :out-lut out-lut
                                     :part2? part2?
                                     :running-score (+ running-score
                                                       (* (second (aref routing-lut (first move) (second move)))
                                                          ;;#'second: animal has already moved.
                                                          (let ((animal-type (aref *world* (second move))))
                                                            (cond ((= 1 animal-type) 1)
                                                                  ((= 2 animal-type) 10)
                                                                  ((= 3 animal-type) 100)
                                                                  ((= 4 animal-type) 1000)
                                                                  (T 0))))))
                   ;;Backtrack for the next valid move.
                   (move-animal :world *world* :from (second move) :to (first move) :destructively? T))))))

(defun 23-solution (&key source part2?)
  "Call #'organize-animals with the needed context."
  (let ((*world* (23-input source :part2? part2? :array? T))
        ;;Start with something worser than the worst case: Assume a part2 world where each of the 16 animals (of type D) moves from node K to node X.
        (*minimal-score* (let* ((part2? T)
                                (lut (routing-lut (23-world part2?) (edges part2?)))
                                (k-to-x-cost (second (aref lut 10 22))))
                           (* 16 1000 k-to-x-cost))))
    (declare (special *minimal-score* *world*))
    (organize-animals
     :routing-lut (routing-lut (23-world part2?) (edges part2?) :array-index-representation? T)
     :in-lut (room-movement-lut :in? T :part2? part2?)
     :out-lut (room-movement-lut :in? nil :part2? part2?)
     :part2? part2?)
    *minimal-score*))

(defmethod solution ((day (eql 23)) (variant (eql 'part1)) source)
  "The cost of the most economical way to organize amphipods which are mapped according to the folded diagram."
  (23-solution :source source :part2? nil))
(is 23 'part1 12521)

(defmethod solution ((day (eql 23)) (variant (eql 'part2)) source)
  "The cost of the most economical way to organize amphipods which are mapped according to the unfolded diagram."
  (23-solution :source source :part2? T))
(defmethod expect ((day (eql 23)) (variant (eql 'part2)))
  44169)
;;Takes as long as all other puzzles together (i.e. some seconds). So what. Let's call it a day.
;;(is 23 'part2 44169)

