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

#|
Parsing!
Parser error handling is the main task, which is why the input parser is kept lenient...
|#

(defrule 10-line (+ (or #\( #\) #\[ #\] #\{ #\} #\< #\>))
  (:lambda (input)
    (text input)))
(parse '10-line "[({(<(())[]>[[{[]{<()<>>")

(defrule 10-file (and (+ (and 10-line (? new))))
  (:lambda (input)
    (let ((entries (elt input 0)))
      (mapcar #'first entries))))

(defmethod input ((day (eql 10)) source)
  "Lists with text containing (+ (or #\( #\) #\[ #\] #\{ #\} #\< #\>))."
  (parse '10-file (read-file day source)))
(input 10 T)
(input 10 'full)


;;;; Solution

;;; These are the parsing rules for the puzzle grammar.

;;The following parsing rule 'chunk is designed to fail at position 0, allowing for a distinction in #'parsing-status.
(defrule chunk (* (or chunk-a chunk-b chunk-c chunk-d)))
(defrule chunk-a (and #\( chunk #\) ))
(defrule chunk-b (and #\[ chunk #\] ))
(defrule chunk-c (and #\{ chunk #\} ))
(defrule chunk-d (and #\< chunk #\> ))

(parse 'chunk "()")
(parse 'chunk "[]")
(parse 'chunk "([])")
(parse 'chunk "{()()()}")
(parse 'chunk "<([{}])>")
(parse 'chunk "[<>({}){}[([])<>]]")
(parse 'chunk "(((((((((())))))))))")

(defun parsing-status (text &optional reverse?)
  "Whether TEXT parsed successfully ('success), is 'incomplete or corrupted, in which case this function returns the score of the unexpected character."
  (let ((position (handler-case
                      (parse (if reverse? 'reversed-chunk 'chunk) text)
                    (error (condition)
                      (esrap-error-position condition))
                    (:no-error (production position success?)
                      (declare (ignore production position success?))
                      'success))))
    (cond ((eql position 'success) position)
          ;;Incomplete: position of error is at the end.
          ((= (length text) position) 'incomplete)
          ;;Corrupted: position of error is before the end; return the score of the unexpected character.
          (T (let ((unexpected-character (elt text position)))
               (cond ((char= unexpected-character (if reverse? #\( #\) )) 3)
                     ((char= unexpected-character (if reverse? #\[ #\] )) 57)
                     ((char= unexpected-character (if reverse? #\{ #\} )) 1197)
                     ((char= unexpected-character (if reverse? #\< #\> )) 25137)))))))
(parsing-status "()")
(parsing-status "(]")
(parsing-status "(")
;;Incomplete incomplete corruped incomplete corrupted corrupted incomplete corrupted corrupted incomplete.
(mapcar #'parsing-status (input 10 T))

(defmethod solution ((day (eql 10)) (variant (eql 'part1)) source)
  "Sum all syntax error scores."
  (reduce #'+ (remove-if-not #'numberp (mapcar #'parsing-status (input day source)))))
(is 10 'part1 26397)

#|
Idea for the part2 task: parsing the reversed string will give the position of the first character that needs to be closed.
Prepend/append the corresponding closing character and recurse.

Comment: Coming from puzzle 15, I realize that this puzzle is inspired by the book "Data structures & algorithms" by Goodrich et al. which lists the required stack-based algorithm for this puzzle in the chapter 5.1.7 "Matching Parentheses and HTML Tags" (p. 205).
|#

(defrule reversed-chunk (* (or reversed-chunk-a reversed-chunk-b reversed-chunk-c reversed-chunk-d)))
(defrule reversed-chunk-a (and #\) reversed-chunk #\( ))
(defrule reversed-chunk-b (and #\] reversed-chunk #\[ ))
(defrule reversed-chunk-c (and #\} reversed-chunk #\{ ))
(defrule reversed-chunk-d (and #\> reversed-chunk #\< ))
#|At >><)(<{][{[[>][))((<({([
           ^ (Line 1, Column 6, Position 6)
...                                      ↑|#
;;(parse 'reversed-chunk (reverse "[({(<(())[]>[[{[]{<()<>>"))

(defun part1score2part2char (score)
  "Return the char associated with the syntax error score (for the reversed part2 task)."
  (cond ((symbolp score) score)
        ((= score 3) #\) )
        ((= score 57) #\] )
        ((= score 1197) #\} )
        ((= score 25137) #\> )
        (T score)))

(defun complete (text gathered-chars)
  "Gather the missing closing parentheses."
  (let ((next-char (part1score2part2char (parsing-status text T))))
    (if (eql 'success next-char)
        (reverse gathered-chars)
        (complete (format nil "~A~A" next-char text) (cons next-char gathered-chars)))))
(complete (reverse "[({(<(())[]>[[{[]{<()<>>") nil)

(defun part2char2part2score (char)
  "Determine the corresponding char from the (reversed) part1 char code."
  (cond ((char= char #\) ) 1)
        ((char= char #\] ) 2)
        ((char= char #\} ) 3)
        ((char= char #\> ) 4)))

(defun puzzle10-linescore (&key (total 0) chars)
  "Determine the score of a line in the second task of puzzle 10."
  (if chars
      (puzzle10-linescore
       :total (+ (* 5 total) (part2char2part2score (first chars)))
       :chars (rest chars))
      total))
(puzzle10-linescore :chars (list #\] #\) #\} #\> ))

(defmethod solution ((day (eql 10)) (variant (eql 'part2)) source)
  "The median of the scores in the puzzle input."
  (let* ((sorted-scores (sort (loop for line in (input day source)
                                    when (eql 'incomplete (parsing-status line))
                                      collect (puzzle10-linescore
                                               :chars (complete (reverse line) nil)))
                              #'<))
         (number-of-scores (length sorted-scores))
         ;;Contract: number-of-scores is odd.
         (middle-position (floor number-of-scores 2)))
    (elt sorted-scores middle-position)))
(is 10 'part2 288957)

