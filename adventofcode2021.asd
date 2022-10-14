;;;; Advent of code 2021 solutions
;;;; Copyright (C) 2022 Benedikt Steger <b.steger@protonmail.ch>
;;;; 
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Affero General Public License along with this program. If not, see <http://www.gnu.org/licenses/>.

(in-package :cl-user)

(defpackage adventofcode2021-asd
  (:use :cl-user :asdf))

(in-package :adventofcode2021-asd)

(defsystem adventofcode2021
  :description "Advent of code 2021 solutions"
  :author "Benedikt Steger"
  :license "AGPLv3+"
  :version "25"
  :serial T
  :depends-on (:asdf :uiop :esrap :cl-fad :minheap :screamer)
  :components ((:module "src"
                :serial T
                :components ((:file "package")
                             (:file "common")
                             (:file "01")
                             (:file "02")
                             (:file "03")
                             (:file "04")
                             (:file "05")
                             (:file "06")
                             (:file "07")
                             (:file "08")
                             (:file "09")
                             (:file "10")
                             (:file "11")
                             (:file "12")
                             (:file "13")
                             (:file "14")
                             (:file "15")
                             (:file "16")
                             (:file "18")
                             (:file "19")
                             (:file "19-detour")
                             (:file "20")
                             (:file "21")
                             (:file "22")
                             (:file "23")
                             (:file "24")
                             (:file "25")
                             (:file "report")))))

