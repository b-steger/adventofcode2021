;;;; Advent of code 2021 solutions
;;;; Copyright (C) 2022 Benedikt Steger <b.steger@protonmail.ch>
;;;; 
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Affero General Public License along with this program. If not, see <http://www.gnu.org/licenses/>.

(in-package :adventofcode2021)

;;;Verify that all examples are still computed correctly.

(eval-when (:execute)
  (loop for day in
        ;;No examples for 17 and 24.
        ;;Attention: value for 21 part2 is effectively cached.
        '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 18 19 20 21 22 23 25)
        collect (list
                 :day day
                 :part1 (is day 'part1 (expect day 'part1) :register-expected-value? nil)
                 :part2 (is day 'part2 (expect day 'part2) :register-expected-value? nil))))

;;; Compute a solution.

(eval-when (:execute)
  (let ((day 25))
    (list :day day :part1 (solution day 'part1 'full) :part2 (solution day 'part2 'full))))

;;; Compute the solutions.

#|
39,829,984,311 processor cycles
4,101,255,536 bytes consed

C-I gives:

#<CONS {1035DB2867}>
--------------------
A proper list:
0: (:DAY 1 :PART1 1681 :PART2 1704)
1: (:DAY 2 :PART1 1868935 :PART2 1965970888)
2: (:DAY 3 :PART1 4147524 :PART2 3570354)
3: (:DAY 4 :PART1 41503 :PART2 3178)
4: (:DAY 5 :PART1 5373 :PART2 21514)
5: (:DAY 6 :PART1 386755 :PART2 1732731810807)
6: (:DAY 7 :PART1 349357 :PART2 96708205)
7: (:DAY 8 :PART1 495 :PART2 1055164)
8: (:DAY 9 :PART1 502 :PART2 1330560)
9: (:DAY 10 :PART1 271245 :PART2 1685293086)
10: (:DAY 11 :PART1 1667 :PART2 488)
11: (:DAY 12 :PART1 3802 :PART2 99448)
12: (:DAY 13 :PART1 621 :PART2 "HKUJGAJZ")
13: (:DAY 14 :PART1 5656 :PART2 12271437788530)
14: (:DAY 15 :PART1 462 :PART2 2846)
15: (:DAY 16 :PART1 957 :PART2 744953223228)
16: (:DAY 18 :PART1 3494 :PART2 4712)
17: (:DAY 19 :PART1 320 :PART2 9655)
18: (:DAY 20 :PART1 5339 :PART2 18395)
19: (:DAY 21 :PART1 412344 :PART2 214924284932572)
20: (:DAY 22 :PART1 647062 :PART2 1319618626668022)
21: (:DAY 23 :PART1 13336 :PART2 53308)
22: (:DAY 24 :PART1 95299897999897 :PART2 31111121382151)
23: (:DAY 25 :PART1 492 :PART2 0)
|#
(eval-when (:execute)
  (time (loop for day in
              '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 18 19 20 21 22 23 24 25)
              collect (list
                       :day day
                       :part1 (solution day 'part1 'full)
                       :part2 (solution day 'part2 'full)))))

