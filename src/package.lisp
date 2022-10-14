;;;; Advent of code 2021 solutions
;;;; Copyright (C) 2022 Benedikt Steger <b.steger@protonmail.ch>
;;;; 
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Affero General Public License along with this program. If not, see <http://www.gnu.org/licenses/>.

(in-package :cl-user) 

(defpackage #:adventofcode2021
  (:use #:common-lisp #:esrap))

(in-package :adventofcode2021)

;;This is for Nikodemus Siivola's HYPERDOC, see <http://common-lisp.net/project/hyperdoc/> and <http://www.cliki.net/hyperdoc>.
(let ((exported-symbols-alist
       (loop for symbol being the external-symbols of :adventofcode2021
             collect (cons symbol
                           (concatenate 'string "#"
                                        (string-downcase symbol))))))
  (defun hyperdoc-lookup (symbol type)
    (declare (ignore type))
    (cdr (assoc symbol exported-symbols-alist :test #'eq)))) 

