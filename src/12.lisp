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

(defrule 12-node (+ (character-ranges (#\a #\z) (#\A #\Z)))
  (:lambda (input)
    ;;List with a name (a symbol) and boolean small?.
    (list (read-from-string (text input)) (every #'lower-case-p input))))
(parse '12-node "start")
(eql (first (parse '12-node "ABC"))
     (first (parse '12-node "ABC")))

(defrule 12-line (and 12-node "-" 12-node)
  (:lambda (input)
    (list (elt input 0) (elt input 2))))
(parse '12-line "start-end")

(defrule 12-file (+ (and 12-line (? new)))
  (:lambda (input)
    (mapcar #'first input)))
(parse '12-file (read-file 12 'toy-example))

(defmethod input ((day (eql 12)) source)
  "A cave system. A cave system seems to have no circles of big caves (in fact, there are no links between big caves obviously)."
  (parse '12-file (read-file 12 source)))
(input 12 'toy-example)
(input 12 'middle-example)
(input 12 T)


;;;; Solution

#|
"Number of ..." in recursive functions: return 1 in the base case.
Only descending if next cave does not form a loop (i.e. a repeated sequence) in visited-list (this seems to be guaranteed).
  Given the insights from parsing, it is obviously possible to just exhaustively traverse the graph.
Part1 puzzle: only descending if next cave is not (and small? in-visited-list).

Transport state through VISITED-list in a recursive function.
|#

(defun once-in-path? (node path)
  "Whether NODE only appears once in PATH."
  (= 1 (count (car node) path :key #'car)))

(defun small-cave? (node)
  "Whether NODE is a small cave."
  (second node))

(defun abides-part2-cave-visiting-rule? (visited)
  "Whether only one node is visited maximally twice in VISITED."
  (let ((tally (make-hash-table)))
    (loop for node in visited
          do (cond ((and (gethash (car node) tally)
                         (small-cave? node))
                    (incf (gethash (car node) tally)))
                   (T (setf (gethash (car node) tally) 1))))
    ;;Only one element can occur twice.
    (>= 1 (loop for v being the hash-value of tally
                summing
                ;;The score.
                (cond
                  ;;Occuring once is not interesting.
                  ((= v 1) 0)
                  ;;Occuring twice should only happen once.
                  ((= v 2) 1)
                  ;;Violation, register violation by contributing to score more than 1.
                  (T 2))))))
;;1 1 1 ok since no number is greater than 1.
(abides-part2-cave-visiting-rule? '((a T) (start T) (b nil)))
;;2 1 ok since only one number is greater than 1.
(abides-part2-cave-visiting-rule? '((a T) (start T) (a T)))
;;3 1 not ok since one number is greater than 2.
(abides-part2-cave-visiting-rule? '((a T) (start T) (a T) (a T)))
;;2 1 2 not ok since more than one number is = 2.
(abides-part2-cave-visiting-rule? '((a T) (start T) (a T) (start T)))

(defun neighbours-of-node (cave-system node-name)
  "The neighbouring nodes of the node with NODE-NAME in a CAVE-SYSTEM."
  (loop for node in cave-system
        when (eql node-name (car (first node)))
          collect (second node)
        when (eql node-name (car (second node)))
          collect (first node)))
(neighbours-of-node (input 12 'toy-example) 'start)
(input 12 'toy-example)

(defun submarine-explore (cave-system start-from-node visited &optional part2-rules?)
  "Follow all possible connected caves. Recursively add 1 once node \"end\" is reached.
   CAVE-SYSTEM is determined by #'input for this day."
  (cond ((eql 'end (caar visited))
         1)
        ((or (and (small-cave? (car visited))
                  ;;Not: we're in a termination condition.
                  (not (if part2-rules?
                           (abides-part2-cave-visiting-rule? visited)
                           (once-in-path? (car visited) visited))))
             (eql 'start (caar visited)))
         0)
        (T
         (loop for n in (neighbours-of-node cave-system start-from-node)
               sum (submarine-explore cave-system (car n) (cons n visited) part2-rules?)))))
;;10
(submarine-explore (input 12 'toy-example) 'start nil)
;;19
(submarine-explore (input 12 'middle-example) 'start nil)
;;226
(submarine-explore (input 12 'example) 'start nil)
;;3802
(submarine-explore (input 12 'full) 'start nil)
;;36
(submarine-explore (input 12 'toy-example) 'start nil T)
;;103
(submarine-explore (input 12 'middle-example) 'start nil T)
;;3509
(submarine-explore (input 12 'example) 'start nil T)

(defmethod solution ((day (eql 12)) (variant (eql 'part1)) source)
  "The number of paths through the cave system (each small cave is visited at most once)."
  (submarine-explore (input day source) 'start nil))
(is 12 'part1 226)

(defmethod solution ((day (eql 12)) (variant (eql 'part2)) source)
  "The number of paths through the cave system (allowing a single small cave to be visited twice)."
  (submarine-explore (input day source) 'start nil T))
(is 12 'part2 3509)

