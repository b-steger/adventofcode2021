;;;; Advent of code 2021 solutions
;;;; Copyright (C) 2022 Benedikt Steger <b.steger@protonmail.ch>
;;;; 
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Affero General Public License along with this program. If not, see <http://www.gnu.org/licenses/>.

(in-package :adventofcode2021)

(defrule new (or (and #\Return #\Newline) #\Return #\Newline))

(defrule white-force (+ #\Space))

(defrule white (* white-force))

(defrule integer (+ (character-ranges (#\0 #\9)))
  (:text x)
  (:lambda (x)
    (parse-integer x)))
(parse 'integer "0123")


(defun read-file (day source)
  "Read the contents of an input file for a given DAY into a string.
   Supported SOURCEs see #'solution."
  (uiop:read-file-string
   (cl-fad:merge-pathnames-as-file
    (user-homedir-pathname)
    "quicklisp/local-projects/adventofcode2021/input/"
    (cond ((or (eql source 'example)
               (eql source T))
           "example/")
          ((eql source 'toy-example) "toy-example/")
          ((eql source 'middle-example) "middle-example/")
          ((eql source 'full) "full/")
          (T (error "Unsupported source.")))
    (format nil "~2,'0D.txt" day))))
;;(read-file 1 T)

;;THINK: A &key in the LAMBDA-LIST is probably a good idea since later puzzles need to twist the input data in multiple ways simultaneously.
(defgeneric input (day source)
  (:documentation "Return input data in a parsed structure ready for processing.
                   DAY is eql-specialized on the day number.
                   Supported sources see #'solution.")
  (:method (day (source (eql T)))
    (input day 'example)))

(defgeneric solution (day variant source)
  (:documentation "Return the solution for a given DAY (eql-specialized on the day number) and VARIANT (specialized on 'part1 or 'part2).
                   Supported sources are 'toy-example 'middle-example 'example and 'full. You can use T as a shorthand for 'example.
                   Toy examples and middle examples are used in the puzzle description in order to describe an aspect of a problem. They are useful for the development of functions which contribute towards a part1 or part2 solution.")
  (:method (day variant (source (eql T)))
    (solution day variant 'example)))

(defgeneric expect (day variant)
  (:documentation "What return value is expected for the example."))

(defun is (day variant expect &key (register-expected-value? T))
  "Register the expected value and execute #'solution before returning and comparing the value."
  (when register-expected-value?
    ;;Yes, this works.
    (defmethod expect ((day (eql day)) (variant (eql variant)))
      expect))
  (let ((solution (solution day variant T)))
    (list :equal? (equal expect solution) :expected expect :solution solution)))

