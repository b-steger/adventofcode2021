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
This puzzle seems similar in spirit to the one of day 6, which also tried to derivate the input as good as possible.
So, how is the problem reduced?
What are the "histogrammable" parts?
The letters, obviously.
Now, what produces a letter?
A pair of letters.
Thought reversal again: a conversion rule not only produces a letter, it also produces two pairs.
"AB -> C" can be read as "AB -> AC,CB" (with the contract that all neighbouring pairs share the same letter, which is explicitly guaranteed).

Manual analysis of the pairs produced by the example reveals that all output pairs of the newly introduced conversion rule appear as a pair on the left side in each original conversion rule.
Which means that the count of a letter is determined by the histogram of pairs (i.e. count the number of times "B" appears on the left side of a pair).
This has to be verified on the input, though.

Adapting the parser to this promising situation...
|#

;;;; Parser

(defrule 14-template (+ (character-ranges (#\A #\Z)))
  (:lambda (input)
    (loop for (a . d) on input
          while d
          collect (read-from-string (format nil "~A~A" a (first d))))))
;;The final count needs to include the righmost character, too. But really only the last character which remains the same throughout the whole execution. Which means that it can be conveniently read from the puzzle input.
;;Count: (NN NC CB) → two N (two symbols begin with N), one C (one symbol begins with C) and B, the rightmost character.
(parse '14-template "NNCB")

(defrule 14-insertion-rule (and (character-ranges (#\A #\Z)) (character-ranges (#\A #\Z)) " -> " (character-ranges (#\A #\Z)))
  (:lambda (input)
    ;;List of left pair, right pair and original left side (matching pair).
    ;;Reasoning for interning as symbols: the input is of manageable size (input is trusted) and SBCL can compare with an #'eq (i.e. a single VOP).
    (list (read-from-string (format nil "~A~A" (elt input 0) (elt input 3)))
          (read-from-string (format nil "~A~A" (elt input 3) (elt input 1)))
          (read-from-string (format nil "~A~A" (elt input 0) (elt input 1))))))
(parse '14-insertion-rule "CH -> B")

(defrule 14-file (and 14-template new new
                      (+ (and 14-insertion-rule (? new))))
  (:lambda (input)
    ;;List of template and hashtable mapping matching-pairs to their expansions (a list of size 2).
    (list (elt input 0)
          (let ((ht (make-hash-table :test 'eq)))
            (loop for rule in (mapcar #'first (elt input 3))
                  do (setf (gethash (third rule) ht) (subseq rule 0 2)))
            ht))))
(parse '14-file "NNCB

CH -> B
HH -> N
")

(defmethod input ((day (eql 14)) source)
  "A polymer template (first item) and a hashtable with insertion rules indexed by the matching pair (second item)."
  (parse '14-file (read-file 14 source)))
(input 14 T)
(input 14 'full)


;;;; Solution

#|
First, check whether left and right pairs appear in the list of the matching pairs.
|#

(defun produced-pairs (input)
  "List of pairs which are produced by the insertion rules."
  (remove-duplicates
   (loop for v being the hash-values of (second input)
         collect (first v)
         collect (second v))))
(produced-pairs (input 14 T))

(defun check-assumption-for-14 (input)
  "Warns if produced pairs do not show up in the list of matching pairs."
  ;;matching-pairs: length 100 (no duplicates).
  (let* ((matching-pairs (loop for k being the hash-keys of (second input)
                                collect k))
         (produced-pairs (produced-pairs input)))
    (unless (subsetp produced-pairs matching-pairs :test #'eq)
      (warn "Assumptions for puzzle 14 do not hold, check the approach of the solution. Continuing nevertheless..."))
    T))
(check-assumption-for-14 (input 14 'full))

#|
The workbench, the histogram.
|#

(defun initial-histogram (input)
  "An histogram with pair symbols as key and 0 as the default value, realized as hash table.
   Initialized with INPUT's polymer template."
  (let ((ht (make-hash-table :test 'eq)))
    (loop for matching-pair in (loop for k being the hash-keys of (second input)
                                     collect k)
          do (setf (gethash matching-pair ht) 0))
    (loop for produced-pair in (produced-pairs input)
          do (setf (gethash produced-pair ht) 0))
    ;;Initialization
    (loop for template-pair in (first input)
          do (incf (gethash template-pair ht)))
    ht))
(initial-histogram (input 14 T))
(initial-histogram (input 14 'full))

#|
What happens during a step?

NNCB = NN NC CB
NCNBCHB = NC CN NB BC CH HB
NBCCNBBBCBHCB = NB BC CC CN NB BB BB BC CB BH HC CB

Each pair contributes two new pairs.
But all existing pairs are destroyed.
Algorithm:
1. Identify all exisiting pairs (i.e. having count > 0).
2. Gather the produced pairs and add them to a fresh histogram (existing-count times).
|#

(defun polymer-expand (production-lut histogram steps-left)
  "Expand the polymer: replace each pair with two pairs according to the insertion rules."
  (if (= 0 steps-left)
      histogram
      (polymer-expand
       production-lut
       (let* ((fresh-histogram (make-hash-table :test 'eq))
              (existing-pairs (loop for k being the hash-key of histogram
                                    for v being the hash-value of histogram
                                    when (> v 0)
                                      collect k))
              ;;Gather the produced pairs...
              (produced-pairs (loop for pair in existing-pairs
                                    collect (list (gethash pair production-lut)
                                                  ;;... and how many times they are introduced.
                                                  (gethash pair histogram)))))
         ;;Add the produced pairs to the histogram.
         (mapcar (lambda (x)
                   (loop for pair in (first x)
                         do (incf (gethash pair fresh-histogram 0) (second x))))
                 produced-pairs)
         fresh-histogram)
       (1- steps-left))))

(defun puzzle-14-result (input steps)
  "Count the number of letters in the expanded-polymer and subtract the least common element count from the most common element count."
  (check-assumption-for-14 input)
  (let* ((expanded-polymer (polymer-expand (second input) (initial-histogram input) steps))
         (tally (make-hash-table)))
    ;;Sum all first elements of pairs (grouped by letter).
    (loop for k being the hash-key of expanded-polymer
          for v being the hash-value of expanded-polymer
          do (incf (gethash (read-from-string (subseq (format nil ":~A" k) 0 2)) tally 0) v))
    ;;The rightmost character of the template survived all transformations and is not reachable by the counting method.
    (incf (gethash (read-from-string
                    (format nil ":~A" (subseq
                                       (format nil "~A" (car (last (first input))))
                                       1)))
                   tally))
    (- (loop for k being the hash-key of tally
             for v being the hash-value of tally
             maximize v)
       (loop for k being the hash-key of tally
             for v being the hash-value of tally
             minimize v))))

(defmethod solution ((day (eql 14)) (variant (eql 'part1)) source)
  "The difference between the most common letter count and the least common letter count, for 10 steps."
  (puzzle-14-result (input 14 source) 10))
(is 14 'part1 1588)

(defmethod solution ((day (eql 14)) (variant (eql 'part2)) source)
  "The difference between the most common letter count and the least common letter count, for 40 steps."
  (puzzle-14-result (input 14 source) 40))
(is 14 'part2 2188189693529)

