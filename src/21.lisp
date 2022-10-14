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

(defrule starting-position (and "Player " integer " starting position: " integer)
  (:lambda (input)
    (elt input 3)))
(parse 'starting-position "Player 1 starting position: 4")

(defrule 21-file (+ (and starting-position (? new)))
  (:lambda (input)
    (mapcar #'first input)))

(defmethod input ((day (eql 21)) source)
  "List denoting the starting positions of the two players."
  (parse '21-file (read-file 21 source)))
(input 21 T)


;;;; Solution

(loop for i from 0 below 210 collect (1+ (mod i 100)))

(defun roll-die (times-rolled)
  "The sum of rolling the deterministic 100-sided die three times."
  (let ((roll (mod times-rolled 100)))
    (reduce #'+ (mapcar (lambda (x) (1+ (mod (+ roll x) 100))) '(0 1 2)))))
(roll-die 0)
;;Externally space 3 (internally starting at 0, externally at 1).
(1+ (mod (+ (1- 8) (roll-die 3)) 10))
(list (roll-die 98)
      (roll-die 99)
      (roll-die 100)
      (roll-die 198)
      (roll-die 199)
      (roll-die 200)
      (roll-die 201)
      (roll-die 202))

(defun move-pawn (start-pos-external number-of-times)
  "Move the pawn on the circular track of size 10."
  (declare (fixnum start-pos-external number-of-times)
           (optimize speed))
  ;;This externally-internally thing is a form of escaping.
  (1+ (mod (+ (the fixnum (1- start-pos-external))
              number-of-times)
           10)))
(move-pawn 7 (+ 2 2 1))

(defun dirac-part1 (&key (current-player 1) (space1 0) (space2 0) (score1 0) (score2 0) (times-rolled 0))
  "Play the Dirac dice game: roll a die three times, move forward rolled times on a circular board of size 10 and add the board position to the score.
   The first player with 1000 points wins.
   The function returns, amongst its final state, the number of times the die was rolled multiplied by the score of the player who did not win.
   CURRENT-PLAYER is the player ID, i.e. either 1 or 2."
  (let* ((roll (roll-die times-rolled))
         (new-space (move-pawn (if (= 1 current-player) space1 space2) roll))
         (new-score (+ (if (= 1 current-player) score1 score2)
                       new-space)))
    (cond
      ((>= new-score 1000)
       (list :current-player current-player
             :space1 space1 :space2 space2
             :score1 score1 :score2 score2
             :times-rolled times-rolled
             :roll roll :new-space new-space :new-score new-score
             ;;3: TIMES-ROLLED remained unchanged even though the player has made it's turn in the meantime.
             :part1-number (* (+ times-rolled 3) (if (= 1 current-player) score2 score1))))
      (T (dirac-part1
          ;;3: Sum of player id 1 and player id 2.
          :current-player (- 3 current-player)
          :space1 (if (= 1 current-player) new-space space1)
          :score1 (if (= 1 current-player) new-score score1)
          :space2 (if (= 2 current-player) new-space space2)
          :score2 (if (= 2 current-player) new-score score2)
          ;;3: The player rolled the die three times.
          :times-rolled (+ 3 times-rolled))))))
(dirac-part1 :space1 (first (input 21 T)) :space2 (second (input 21 T)))

(defmethod solution ((day (eql 21)) (variant (eql 'part1)) source)
  "Play the Dirac dice game until someone wins. Return the score of the player not winning multiplied by the number of times the die was rolled."
  (let ((input (input day source)))
    (getf (dirac-part1
           :space1 (first input)
           :space2 (second input))
          :part1-number)))
(is 21 'part1 739785)
;;412344
(find (solution 21 'part1 'full)
      (loop for i from 1 below 11
            append (loop for j from 1 below 11
                         collect (getf (dirac-part1 :space1 i :space2 j) :part1-number))))

;;; Part2

#|
Playing 444356092776315 + 341960390180808 = 786316482957123 games is computationally infeasible, or playing the game 786316482957123 times is not the intention of the puzzle at least. This puzzle must reach the number in a different way and consequently shares similarities with day 6 and day 14. Nevertheless, the tree of dice rolls, which I want to realize through recursive calls, must be visited exhaustively, since every outcome is requested explicitly. This torpedoes any game tree pruning approaches.

An analysis of all possible outcomes in the part1 games shows that 30 out of 100 scores are duplicates:
|#

;;=> (100 70)
(let ((possible-part1-games
        (loop for i from 1 below 11
              append (loop for j from 1 below 11
                           collect (getf (dirac-part1 :space1 i :space2 j) :part1-number)))))
  (list (length possible-part1-games)
        (length (remove-duplicates possible-part1-games))))

#|
As a consequence, a reduction must be possible.

Idea: the combinations that are requested are partially subsumed under a single turn. A 1-2-3 is the same as a 1-3-2, as it results in the same progress on the board in the same atomic turn.
|#

(let* ((die-rolls (loop for i from 1 below 4
                        append (loop for j from 1 below 4
                                     append (loop for k from 1 below 4
                                                  collect (+ i j k)))))
       (expected-number-of-combinations (expt 3 3))
       (effective-number-of-combinations (length die-rolls))
       (game-tree-leaves (sort (remove-duplicates die-rolls) #'<))
       (roll-sum-tally (mapcar (lambda (x)
                                 (count-if (lambda (y) (= x y)) die-rolls))
                               game-tree-leaves)))
  (list expected-number-of-combinations
        effective-number-of-combinations
        game-tree-leaves
        roll-sum-tally))

(defparameter *all-possible-dirac-3-dice-rolls*
  ;;'(1 3 6 7 6 3 1): roll-sum-tally in previous form.
  (loop with game-tree-leaves = '(1 3 6 7 6 3 1)
        ;;3 and 10: rebuilding game-tree-leaves from previous form.
        for i from 3 below 10
        collect (list i (elt game-tree-leaves (- i 3))))
  "List containing lists with the sum a player can throw in a single turn and the combinatorial share (tally).")

(declaim (ftype (function
                 (&key (:rolled-sum fixnum)
                       (:player1current? boolean)
                       (:space1 fixnum)
                       (:space2 fixnum)
                       (:score1 fixnum)
                       (:score2 fixnum)
                       (:depth-factor fixnum))
                 null)
                dirac-part2))
(defun dirac-part2 (&key
                      rolled-sum (player1current? T) (space1 0) (space2 0) (score1 0) (score2 0)
                      ;;Combine the tallies by multiplying them.
                      (depth-factor 1))
  "Construct the dirac game tree and count the number of wins for each player.
   Expects a context which is set up by #'21-solution-part2."
  (declare (special *player1sum* *player2sum*)
           (optimize speed))
  (let* ((new-space (the fixnum (move-pawn (if player1current? space1 space2) rolled-sum)))
         (new-score (+ (if player1current? score1 score2)
                       new-space)))
    (cond ((>= new-score 21)
           ;;The current player has won this game.
           (if player1current?
               (incf (the fixnum *player1sum*) depth-factor)
               (incf (the fixnum *player2sum*) depth-factor)))
          (T (loop for (sum tally) in *all-possible-dirac-3-dice-rolls*
                   do (dirac-part2 :rolled-sum sum
                                   :player1current? (not player1current?)
                                   :space1 (if player1current? new-space space1)
                                   :score1 (if player1current? new-score score1)
                                   :space2 (if player1current? space2 new-space)
                                   :score2 (if player1current? score2 new-score)
                                   :depth-factor (* (the (integer 1 7) tally)
                                                    (the (integer 1 99999999999) depth-factor)))))))
  nil)

(defun 21-solution-part2 (input)
  "Set up the context for #'dirac-part2 and return the winner game tree size."
  (let* ((space1 (first input))
         (space2 (second input))
         (*player1sum* 0)
         (*player2sum* 0))
    (declare (special *player1sum* *player2sum*)
             (fixnum space1 space2 *player1sum* *player2sum*))
    (loop for (sum tally) in *all-possible-dirac-3-dice-rolls*
          do (dirac-part2 :rolled-sum sum
                          :player1current? T
                          :space1 space1
                          :space2 space2
                          :depth-factor tally))
    (max *player1sum* *player2sum*)))

(defparameter *puzzle-21-solutions*
  (make-array
   '(100)
   :initial-contents
   '(32491093007709 27674034218179 48868319769358
     97774467368562 138508043837521 157253621231420
     141740702114011 115864149937553 85048040806299
     57328067654557 27464148626406 24411161361207
     45771240990345 93049942628388 131888061854776
     149195946847792 133029050096658 106768284484217
     76262326668116 49975322685009 51863007694527
     45198749672670 93013662727308 193753136998081
     275067741811212 309991007938181 273042027784929
     214368059463212 147573255754448 92399285032143
     110271560863819 91559198282731 193170338541590
     404904579900696 575111835924670 647608359455719
     568867175661958 444356092776315 303121579983974
     187451244607486 156667189442502 129742452789556
     274195599086465 575025114466224 816800855030343
     919758187195363 807873766901514 630947104784464
     430229563871565 265845890886828 175731756652760
     146854918035875 309196008717909 647920021341197
     920342039518611 1036584236547450 911090395997650
     712381680443927 486638407378784 301304993766094
     152587196649184 131180774190079 272847859601291
     570239341223618 809953813657517 912857726749764
     803934725594806 630797200227453 433315766324816
     270005289024391 116741133558209 105619718613031
     214924284932572 446968027750017 634769613696613
     716241959649754 632979211251440 499714329362294
     346642902541848 218433063958910 83778196139157
     75823864479001 148747830493442 306621346123766
     435288918824107 492043106122795 437256456198320
     348577682881276 245605000281051 157595953724471
     56852759190649 49982165861983 93726416205179
     190897246590017 270803396243039 306719685234774
     274291038026362 221109915584112 158631174219251
     104001566545663))
  "Precomputed solutions for the puzzle 21.")

(defmethod solution ((day (eql 21)) (variant (eql 'part2)) source)
  "The size of the winning dirac game tree when a three-sided die is considered."
  ;;(21-solution-part2 (input day source))
  (let ((input (input day source)))
    (aref *puzzle-21-solutions* (+ (* (1- (first input)) 10)
                                   (1- (second input))))))
(is 21 'part2 444356092776315)

