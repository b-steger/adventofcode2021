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

(defrule 18-pair (and "[" (or integer 18-pair) "," (or integer 18-pair) "]")
  (:lambda (input)
    (let ((x (elt input 1))
          (y (elt input 3)))
      (list x y))))
(parse '18-pair "[1,2]")
(parse '18-pair "[[1,2],3]")
(parse '18-pair "[9,[8,7]]")
(parse '18-pair "[[1,9],[8,5]]")
(parse '18-pair "[[[[1,2],[3,4]],[[5,6],[7,8]]],9]")
(parse '18-pair "[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]")
(parse '18-pair "[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]")

(defrule 18-file (+ (and 18-pair (? new)))
  (:lambda (input)
    (mapcar #'first input)))

(defmethod input ((day (eql 18)) source)
  "List of binary trees with leaves being integers [0..9]."
  (parse '18-file (read-file day source)))
(input 18 T)
(input 18 'full)


;;;; Solution

#|
The reduce operation profits from a binary tree with a doubly linked list at the leaves.
This solution converts the tree to a flat representation which is suited for the operations required by the spec.
|#

(defun flat-tree (nested-tree &key (height 0))
  "Serialize NESTED-TREE.
   Returns a list of preorder-traversed leaves of binary NESTED-TREE enriched with tree height information."
  (if (listp nested-tree)
      (append (flat-tree (first nested-tree) :height (1+ height))
              (flat-tree (second nested-tree) :height (1+ height)))
      ;;Two #'list: #'append operation in the recursive case.
      (list (list :value nested-tree :height height))))
;;Heights 5 5 4 3 2 1.
(flat-tree (parse '18-pair "[[[[[9,8],1],2],3],4]"))
;;Heights 1 2 3 4 5 5.
(flat-tree (parse '18-pair "[7,[6,[5,[4,[3,2]]]]]"))
;;Heights 2 3 4 5 5 1.
(flat-tree (parse '18-pair "[[6,[5,[4,[3,2]]]],1]"))
;;Heights 2 3 4 5 5 2 3 4 5 5.
(flat-tree (parse '18-pair "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]"))
#|Heights 2 3 4 4 2 3 4 5 5.
Flat trees look like this:
=> ((:value 3 :height 2) (:value 2 :height 3) (:value 8 :height 4)
    (:value 0 :height 4) (:value 9 :height 2) (:value 5 :height 3)
    (:value 4 :height 4) (:value 3 :height 5) (:value 2 :height 5))|#
(flat-tree (parse '18-pair "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"))

(defun nested-tree (flat-tree)
  "Unserialize FLAT-TREE.
   Returns the binary tree represented by the leaves list in FLAT-TREE, as produced by #'flat-tree."
  ;;There must always be at least one pair with equal height. Replace them by an entry with a list of their :value slots in its value slot.
  (labels ((n (flat-tree-rest)
             "Group the deepest (and first) nested pair with equal height."
             (if (cdr flat-tree-rest)
                 (let ((max-height (loop for item in flat-tree-rest
                                         maximize (getf item :height))))
                   (if (and (= (getf (car flat-tree-rest) :height)
                               (getf (cadr flat-tree-rest) :height))
                            ;;Go for the deepest pair first.
                            (>= (getf (car flat-tree-rest) :height)
                                max-height))
                       (append (list (list :value (list (getf (car flat-tree-rest) :value)
                                                        (getf (cadr flat-tree-rest) :value))
                                           :height (1- (getf (car flat-tree-rest) :height))))
                               (cddr flat-tree-rest))
                       (append (list (car flat-tree-rest))
                               (n (cdr flat-tree-rest)))))
                 flat-tree-rest)))
    (let ((grouped (n flat-tree)))
      ;;#'cddr: more than two elements in FLAT-TREE (i.e. not a tree yet).
      (if (cddr flat-tree)
          (nested-tree grouped)
          (getf (first grouped) :value)))))
;;Target ((3 (2 (8 (7 6)))) ((((1 2) (3 4)) 2) 2))
(nested-tree (flat-tree (parse '18-pair "[[3,[2,[8,[7,6]]]],[[[[1,2],[3,4]],2],2]]")))
(nested-tree (flat-tree (parse '18-pair "[1,[2,[3,[4,[5,[6,7]]]]]]")))
(nested-tree (flat-tree (parse '18-pair "[[[[0,7],4],[15,[0,13]]],[1,1]]")))
(nested-tree (flat-tree (parse '18-pair "[[[[4,2],2],6],[8,7]]")))

(defun redistribute-height-5 (flat-tree-rest &key (first-time? T))
  "If a pair is inside four pairs (tree height = 5), add its left component to its previous pre-order neighbour and its right component to its next pre-order neighbour (if possible) and leave a 0 in place.
   Operates on a flat tree representation, see #'flat-tree."
  (if (cdr flat-tree-rest)
      ;;Recursive case.
      (cond
        ;;Special recursive case: the first two entries are both of height 5.
        ((and first-time?
              (= 5 (getf (car flat-tree-rest) :height))
              (= 5 (getf (cadr flat-tree-rest) :height)))
         ;;Ignore the left value and dissolve the cadr into the caddr.
         (append (list (list :value 0 :height (1- (getf (cadr flat-tree-rest) :height)))
                       (list :value (+ (getf (cadr flat-tree-rest) :value)
                                       (getf (caddr flat-tree-rest) :value))
                             :height (getf (caddr flat-tree-rest) :height)))
                 (redistribute-height-5 (subseq flat-tree-rest (min 3 (length flat-tree-rest))) :first-time? nil)))
        ;;Normal recursive case: 5 5 found somewhere else in the list.
        ((and first-time?
              (= 5 (getf (cadr flat-tree-rest) :height))
              ;;Actually redundant; there must be/will be a cddr with height 5.
              (= 5 (getf (caddr flat-tree-rest) :height)))
         (remove nil
                 (append (list
                          ;;The current is the left one; add the value of the cadr.
                          (list :value (+ (getf (car flat-tree-rest) :value)
                                          (getf (cadr flat-tree-rest) :value))
                                :height (getf (car flat-tree-rest) :height))
                          (list :value 0 :height (1- (getf (cadr flat-tree-rest) :height)))
                          ;;Also move the value of the caddr to the cadddr.
                          (when (cadddr flat-tree-rest)
                            (list :value (+ (getf (caddr flat-tree-rest) :value)
                                            (getf (cadddr flat-tree-rest) :value))
                                  :height (getf (cadddr flat-tree-rest) :height))))
                         (redistribute-height-5
                          (subseq flat-tree-rest
                                  ;;4: The next and next after the next disappeared, and even the entry after this is already processed, too.
                                  ;;There may or may not be elements left.
                                  (min 4 (length flat-tree-rest)))
                          :first-time? nil))))
        ;;No 5 5 found.
        (T (append (list (car flat-tree-rest)) (redistribute-height-5 (rest flat-tree-rest) :first-time? first-time?))))
      ;;Base case.
      flat-tree-rest))
(redistribute-height-5 nil)
;;Target ((((0 9) 2) 3) 4)
(nested-tree (redistribute-height-5 (flat-tree (parse '18-pair "[[[[[9,8],1],2],3],4]"))))
;;Target (7 (6 (5 (7 0))))
(nested-tree (redistribute-height-5 (flat-tree (parse '18-pair "[7,[6,[5,[4,[3,2]]]]]"))))
;;Target ((6 (5 (7 0))) 3)
(nested-tree (redistribute-height-5 (flat-tree (parse '18-pair "[[6,[5,[4,[3,2]]]],1]"))))
;;Target ((3 (2 (8 0))) (9 (5 (4 (3 2)))))
(nested-tree (redistribute-height-5 (flat-tree (parse '18-pair "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]"))))
;;Target ((3 (2 (8 0))) (9 (5 (7 0))))
(nested-tree (redistribute-height-5 (flat-tree (parse '18-pair "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"))))
(nested-tree (redistribute-height-5 (redistribute-height-5 (flat-tree (parse '18-pair "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")))))

(defun split-on-10-or-greater (flat-tree-rest &key (first-time? T))
  "Replace each element having a value >= 10 in FLAT-TREE-REST with a list of two items.
   New values are the old number divided by two, rounded down and up, respectively."
  (if flat-tree-rest
      (if (and first-time?
               (< 9 (getf (car flat-tree-rest) :value)))
          (let ((value (getf (car flat-tree-rest) :value))
                (height (getf (car flat-tree-rest) :height)))
            (append (list (list :value (floor (/ value 2)) :height (1+ height))
                          (list :value (ceiling (/ value 2)) :height (1+ height)))
                    (split-on-10-or-greater (rest flat-tree-rest) :first-time? nil)))
          (cons (car flat-tree-rest)
                (split-on-10-or-greater (rest flat-tree-rest) :first-time? first-time?)))
      flat-tree-rest))
;;Target ((((0 7) 4) ((7 8) (0 13))) (1 1))
(nested-tree (split-on-10-or-greater (flat-tree (parse '18-pair "[[[[0,7],4],[15,[0,13]]],[1,1]]"))))
;;Nothing should happen. → Target ((((0 7) 4) (5 (0 3))) (1 1))
(nested-tree (split-on-10-or-greater (flat-tree (parse '18-pair "[[[[0,7],4],[5,[0,3]]],[1,1]]"))))
;;The last element, too.
(nested-tree (split-on-10-or-greater '((:value 0 :height 4) (:value 10 :height 4))))

(defun every-height-less-than-5? (flat-tree)
  "Whether every leaf in FLAT-TREE has a height less than 5."
  (every (lambda (x)
           (< (getf x :height) 5))
         flat-tree))

(defun every-value-less-than-10? (flat-tree)
  "Whether every leaf in FLAT-TREE has a value less than 10."
  (every (lambda (x)
           (< (getf x :value) 10))
         flat-tree))

(defun reduce-tree (flat-tree)
  "Alternately redistribute leaves with height 5 and split on leaves with value 10 or greater until FLAT-TREE is reduced."
  (cond ((not (every-height-less-than-5? flat-tree))
         (reduce-tree (redistribute-height-5 flat-tree)))
        ((not (every-value-less-than-10? flat-tree))
         (reduce-tree (split-on-10-or-greater flat-tree)))
        (T flat-tree)))
(nested-tree (reduce-tree (flat-tree (parse '18-pair "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]"))))

(defun add-sn (&optional left right)
  "Adds two snailfish numbers (nested lists)."
  ;;Conversion round trip actually not needed: can increase the heights and append both flat trees.
  (nested-tree
   (reduce-tree
    (flat-tree
     ;;(cons left (cons right nil))
     (list left right)))))
;;Target ((((0 7) 4) ((7 8) (6 0))) (8 1))
(add-sn (parse '18-pair "[[[[4,3],4],4],[7,[[8,4],9]]]") (parse '18-pair "[1,1]"))

;;Target (((1 1) (2 2)) (3 3)) (4 4))
(reduce #'add-sn (parse '18-file "[1,1]
[2,2]
[3,3]
[4,4]
"))
;;Target ((((3 0) (5 3)) (4 4)) (5 5))
(reduce #'add-sn (parse '18-file "[1,1]
[2,2]
[3,3]
[4,4]
[5,5]
"))
;;Target ((((5 0) (7 4)) (5 5)) (6 6))
(reduce #'add-sn (parse '18-file "[1,1]
[2,2]
[3,3]
[4,4]
[5,5]
[6,6]
"))

;;Target (((8 7) (7 7)) ((8 6) (7 7))) (((0 7) (6 6)) (8 7)))
(reduce #'add-sn (input 18 T))

(defun magnitude (nested-tree)
  "The recursive and weighted sum of NESTED-TREE."
  (+ (* 3 (if (listp (first nested-tree))
              (magnitude (first nested-tree))
              (first nested-tree)))
     (* 2 (if (listp (second nested-tree))
              (magnitude (second nested-tree))
              (second nested-tree)))))
(list
 ;;29
 (magnitude (parse '18-pair "[9,1]"))
 ;;21
 (magnitude (parse '18-pair "[1,9]"))
 ;;129
 (magnitude (parse '18-pair "[[9,1],[1,9]]"))
 ;;143
 (magnitude (parse '18-pair "[[1,2],[[3,4],5]]"))
 ;;1384
 (magnitude (parse '18-pair "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"))
 ;;445
 (magnitude (parse '18-pair "[[[[1,1],[2,2]],[3,3]],[4,4]]"))
 ;;791
 (magnitude (parse '18-pair "[[[[3,0],[5,3]],[4,4]],[5,5]]"))
 ;;1137
 (magnitude (parse '18-pair "[[[[5,0],[7,4]],[5,5]],[6,6]]"))
 ;;3488
 (magnitude (parse '18-pair "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"))
 ;;4140
 (magnitude (reduce #'add-sn (parse '18-file "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]
"))))

(defmethod solution ((day (eql 18)) (variant (eql 'part1)) source)
  "The weighted sum of the reduced snailfish numbers."
  (magnitude (reduce #'add-sn (input day source))))
(is 18 'part1 3488)

#|
Solution for part2: cross join.
|#

(defmethod solution ((day (eql 18)) (variant (eql 'part2)) source)
  "The largest weighted sum between each possible input line combination in SOURCE.
   Excludes comparisons of each line with itself."
  (let ((source (input day source)))
    (loop for outer in source
          maximize (loop for inner in source
                         unless (equal outer inner)
                           maximize (magnitude (reduce #'add-sn (list outer inner)))))))
;;3993
(solution 18 'part2 'middle-example)
(is 18 'part2 3805)

