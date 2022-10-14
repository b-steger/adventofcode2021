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

(defrule 04-draws (+ (and integer (? #\,)))
  (:lambda (input)
    (mapcar #'first input)))
(parse '04-draws "1,2,3")

(defrule 04-board-line (and white
                            integer white-force
                            integer white-force
                            integer white-force
                            integer white-force
                            integer)
  (:lambda (input)
    (let ((first (elt input 1))
          (second (elt input 3))
          (third (elt input 5))
          (fourth (elt input 7))
          (fifth (elt input 9)))
      ;;THINK: vector? → YAGNI
      (list first second third fourth fifth))))
(parse '04-board-line "  22 13       17 11 0")

(defrule 04-board (and 04-board-line new
                       04-board-line new
                       04-board-line new
                       04-board-line new
                       04-board-line)
  (:lambda (input)
    (let ((first (elt input 0))
          (second (elt input 2))
          (third (elt input 4))
          (fourth (elt input 6))
          (fifth (elt input 8)))
      (list first second third fourth fifth))))
(parse '04-board "22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19")

(defrule 04-file (and 04-draws new new
                      (+ (and 04-board (? (+ new)))))
  (:lambda (input)
    (let ((draws (elt input 0))
          (boards (elt input 3)))
      (list draws (mapcar #'first boards)))))

(defmethod input ((day (eql 4)) source)
  "Returns the list (list draws boards)"
  (parse '04-file (read-file day source)))
(input 4 T)
(input 4 'full)


;;;; Solution

(defun marked? (number)
  "Whether a NUMBER is marked (>=1000) or not."
  (>= number 1000))

(defun mark! (board number)
  "Mark all elements in BOARD which are #'= to NUMBER, i.e. increase it by 1000."
  (loop for row in board
        collect (loop for col in row
                      collect (if (= number col)
                                  (incf col 1000)
                                  col))))
(mark! (first (second (input 4 T))) 7)

(defun mark-boards! (boards number)
  "Apply #'mark to all BOARDS."
  (loop for board in boards
        collect (mark! board number)))
(mark-boards! (second (input 4 T)) 7)

(defun row-wins? (board row)
  "Whether all numbers in a ROW (offset by 0) are marked."
  (every #'marked?
         (elt board row)))
(let ((a-board '((22 13 17 11 0)
                 (1008 1002 1023 1004 1024)
                 (21 9 14 16  7)
                 (6 10  3 18  5)
                 (1 12 20 15 19))))
  (list (row-wins? a-board 0) (row-wins? a-board 1)))

(defun column-wins? (board column)
  "Whether all numbers in a COLUMN (offset by 0) are marked."
  (every #'marked?
         (loop for row in board collect (elt row column))))
(let ((a-board '((22 1013 17 11 0)
                 (8 1002 23 4 24)
                 (21 1009 14 16  7)
                 (6 1010  3 18  5)
                 (1 1012 20 15 19))))
  (list (column-wins? a-board 0) (column-wins? a-board 1)))

(defun board-wins? (board)
  "Whether a row or a column of BOARD consists solely of marked numbers."
  (loop for i from 0 below 5
        do (when (or (row-wins? board i)
                     (column-wins? board i))
             (return T))))
(let ((a-board '((22 1013 17 11 0)
                 (8  1002 23 4 24)
                 (21 1009 14 16  7)
                 (6  1010  3 18  5)
                 (1  1012 20 15 19))))
  (board-wins? a-board))

(defun score (board number-called)
  "Sum all unmarked numbers in a BOARD and multiply this sum by NUMBER-CALLED."
  (* (loop for row in board
           summing (loop for x in row
                         unless (marked? x)
                           summing x))
     number-called))
(score '((14 21 17 24  4)
         (10 16 15  9 19)
         (18  8 23 26 20)
         (22 11 13  6  5)
         ( 2  0 12  3  7))
       24)

(defun draw-number (remaining-draws boards old-draw)
  "Unless a winning board is found, go through a list of draws and apply it to BOARDS. Defined recursively."
  (let ((winner (find-if #'board-wins?
                         boards)))
    (or (when winner (list winner old-draw))
        (if (cdr remaining-draws)
            ;;Recursive case.
            (draw-number (cdr remaining-draws)
                         (mark-boards! boards (first remaining-draws))
                         (first remaining-draws))
            ;;Base case.
            (list (mark-boards! boards (car remaining-draws)) (car remaining-draws))))))
(let ((input (input 4 T)))
  (draw-number (first input) (second input) (first (first input))))

(defmethod solution ((day (eql 4)) (variant (eql 'part1)) source)
  "The score of the first winning bingo board."
  (let* ((input (input 4 source))
         (draws (first input))
         (boards (second input))
         (winner-board (draw-number draws boards (first draws))))
    ;;Alternative: #'multiple-value-bind.
    (score (first winner-board) (second winner-board))))
(is 4 'part1 4512)

(defun draw-number-part2 (remaining-draws boards old-draw old-winner last-winner-draw)
  "Determine the last board which wins."
  (let* ((a-winner-in-list (find-if #'board-wins?
                                    boards))
         (a-winner (or a-winner-in-list
                       old-winner))
         ;;Remove winning boards before descending.
         (boards (remove-if #'board-wins? boards)))
    (if remaining-draws
        ;;Recursive case.
        (draw-number-part2 (cdr remaining-draws)
                           (mark-boards! boards (first remaining-draws))
                           (first remaining-draws)
                           a-winner
                           ;;Retain the called number of the last winner.
                           (if a-winner-in-list
                               old-draw
                               last-winner-draw))
        ;;Base case.
        (list a-winner last-winner-draw))))

(defmethod solution ((day (eql 4)) (variant (eql 'part2)) source)
  "The score of the last winning bingo board."
  (let* ((input (input 4 source))
         (draws (first input))
         (boards (second input))
         (winner-board (draw-number-part2 draws boards (first draws) nil nil)))
    (score (first winner-board) (second winner-board))))
(is 4 'part2 1924)

