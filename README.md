
Common Lisp solutions for the advent calendar "Advent of code 2021".


# Highlights

* Day 6: There are [1.3*10<sup>972438</sup>](src/06-num-of-fishes-after-25699999-days.txt) fishes in the example ocean after 25699999 days.
* Day 8: See a frequency analysis in action for a substitution cipher.
* Day 10: Exploiting conditions for a "reverse recursion".
* Day 16: 1. The function terminal in the parser rules. 2. The ```#'evaluate-packet``` method which is eql-specialized on 4.
* Day 22: ```CREATE INDEX ON input USING irregularly_decomposing_binary_region_octree (linearly_scaled_geom) TABLESPACE dev_null;```
* Day 24: A possibility to leverage Common Lisp's meta-capabilities! To some non-indefinite extent.


# Some notes

* I deem documentation important. As a consequence, my code represents a communicative workbench with which others should get productive as fast as possible. Simultaneously, I want to convey how nicely the incremental development style of Common Lisp is integrating with the problem solving aspect of these puzzles.

* As computer time costs less than programmer time, I did not really focus on execution speed. I focussed on clarity instead, which resulted in increased development speed. Only few places needed some performance-related attention. In general, I am happy with my chosen data structures and the running time of my algorithms for the given input size.

* Puzzles 24, 16, 10, 22 and 8 made most fun. Puzzle 19 was the hardest one. Maybe I missed the logical shortcut.

* Except for my DSL library lisp-at-work, I did not encounter a chance to write useful Lisp macros in my Lisp projects yet. Even this project saw some meta-stuff in day 24 **only**.


# Code statistics/housekeeping

Made with the help of my DSL library lisp-at-work.

Report specialized on DSL: Common Lisp (ANSI INCITS 226-1994[S20018]).
* Code base hash: 1370420509405277170.
* File count: 118.
* Number of top-level forms: 831.
* Number of comment lines: 768 (427 unique comments).
* Comment line length average: 76.2 characters.
* Code base has 175 defuns, 4 defgenerics and 90 defmethods (0 colliding with defuns), 1 labels, 7 flets and 363 variables defined by #'let and #'let*.
* Most popular symbols which begin forms (LIMIT 20): input (380), * (351), list (242), if (221), eql (205), parse (180), loop (176), defun (175), = (162), first (155), aref (147), and (126), let (120), getf (117), + (112), lambda (108), elt (108), second (97), defmethod (90), defrule (87).
* Most nested form is in the file src/16.lisp.
* The four biggest forms: ```(defun rotate-vector ...)``` in src/19.lisp, ```(defun manually-interpret-aoc24-alu ...)``` in src/24.lisp, ```(defun valid-moves ...)``` in src/23.lisp, ```(defun block-contribution ...)``` in src/22.lisp.
* Average number of return values across all defun, defmethod, lambda, labels and flet forms: 1.018.
* Number of recursive functions, i.e. defuns with enclosed forms that call the respective function name currently: 42.
* Greatest cyclomatic complexity which considers loop, do, do*, dotimes, dolist, the map-family and format with iteration directives: 5 in src/23.lisp.
* Defun name collisions: ().
* Exported symbols: ().
* Prefixed symbols: (cl-fad:merge-pathnames-as-file pairing-heap:decrease-key pairing-heap:empty-p pairing-heap:extract-min pairing-heap:insert pairing-heap:pairing-heap ql:quickload screamer:a-member-of screamer:all-values screamer:either screamer:fail screamer:local uiop:read-file-string).
* Symbols with double package markers: (screamer::defun pairing-heap::node).


# Setup

Copy this folder into $HOME/quicklisp/local-projects and call ```(ql:quickload "adventofcode2021")``` in example/setup.lisp (C-c C-c).


# Editor support

Those who use an editor which is capable of highlighting matching parentheses and which is capable of indenting wrapped lines profit from reduced distraction. SLIME, which operates in Emacs, makes smart completion suggestions and makes form operations easy, which is why Common Lisp code editing is efficient. I really recommend it. Here is an excerpt from my .emacs:

```
(custom-set-variables
 '(show-paren t)
 '(show-paren-mode t)
 '(indent-tabs-mode nil)
 '(word-wrap t))

;;Debian package elpa-adaptive-wrap
(add-hook 'text-mode-hook 'adaptive-wrap-prefix-mode)
(add-hook 'prog-mode-hook 'adaptive-wrap-prefix-mode)
```

Some forms do not have side effects. They are left intentionally in the files because they help a lot during code reading. Use C-M-x (```slime-eval-defun```) to see the evaluation result or use C-I (```slime-inspect```) if you want to inspect the results.

As a matter of fact, I began to include some returned values since I think that the files get read, not executed.


# License

Advent of code 2021 solutions
Copyright (C) 2022 Benedikt Steger <b.steger@protonmail.ch>

This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License along with this program. If not, see <http://www.gnu.org/licenses/>.


For the license terms of uiop/cl-fad/esrap/minheap/screamer consult the respective files in the Quicklisp packages.


# Mirrors

https://github.com/b-steger/adventofcode2021/

