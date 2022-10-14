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

(defrule command-forward (and "forward " integer)
  (:lambda (input)
    (let ((amount (elt input 1)))
      (list 'forward amount))))
(parse 'command-forward "forward 2")

(defrule command-down (and "down " integer)
  (:lambda (input)
    (let ((amount (elt input 1)))
      (list 'down amount))))
(parse 'command-down "down 2")

(defrule command-up (and "up " integer)
  (:lambda (input)
    (let ((amount (elt input 1)))
      (list 'up amount))))
(parse 'command-up "up 2")

(defrule 02-file (+ (and (or command-forward command-down command-up) (? new)))
  (:lambda (input)
    (mapcar #'first input)))

(defmethod input ((day (eql 2)) source)
  (parse '02-file (read-file day source)))
(input 2 T)


;;;; Solution

(defclass part1state ()
  ((h-pos :initarg :h-pos :initform nil :accessor h-pos)
   (depth :initarg :depth :initform nil :accessor depth)))

(defmethod forward! ((state part1state) units)
  (incf (h-pos state) units))

(defmethod down! ((state part1state) units)
  (incf (depth state) units))

(defmethod up! ((state part1state) units)
  (decf (depth state) units))

(defun 02-solution (day source state)
  "Apply commands to STATE (an instance of part1state or part2state) and return the updated score."
  (loop for command in (input day source)
        do (cond ((eq 'forward (car command))
                  (forward! state (cadr command)))
                 ((eq 'up (car command))
                  (up! state (cadr command)))
                 ((eq 'down (car command))
                  (down! state (cadr command)))))
  (* (h-pos state)
     (depth state)))
(02-solution 2 'example (make-instance 'part1state :h-pos 0 :depth 0))

(defmethod solution ((day (eql 2)) (variant (eql 'part1)) source)
  "Call #'02-solution with an instance of part1state."
  (02-solution day source (make-instance 'part1state :h-pos 0 :depth 0)))
(is 2 'part1 150)

(defclass part2state (part1state)
  ((aim :initarg :aim :initform nil :accessor aim)))

(defmethod forward! ((state part2state) units)
  (incf (h-pos state) units)
  (incf (depth state) (* (aim state) units)))

(defmethod down! ((state part2state) units)
  (incf (aim state) units))

(defmethod up! ((state part2state) units)
  (decf (aim state) units))

(defmethod solution ((day (eql 2)) (variant (eql 'part2)) source)
  "Call #'02-solution with an instance of part2state."
  (02-solution day source (make-instance 'part2state :h-pos 0 :depth 0 :aim 0)))
(is 2 'part2 900)

