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

(defrule 16-hex-letter (character-ranges (#\0 #\9) (#\a #\z) (#\A #\Z))
  (:lambda (input)
    (format nil "~4,'0B" (parse-integer (text input) :radix 16))))
(parse '16-hex-letter "0")

(defrule 16-hex (and (+ 16-hex-letter) (? new))
  (:lambda (input)
    ;;Can not distinguish junk from data without parsing from the start. → Use :junk-allowed T. → Design rules with :junk-allowed T in mind.
    ;;(string-right-trim '(#\0) (text input))
    (text (elt input 0))))
(parse '16-hex "38006F45291200")
(parse '16-hex "38006F45291200
")

(defrule bit (or #\0 #\1))

(defrule packet (or packet-literal packet-operator))

(defrule packet-literal-group-continuing (and "1" bit bit bit bit)
  (:lambda (input)
    (subseq (text input) 1)))
(parse 'packet-literal-group-continuing "10111")

(defrule packet-literal-group-finishing (and "0" bit bit bit bit)
  (:lambda (input)
    (subseq (text input) 1)))
(parse 'packet-literal-group-finishing "00101")

(defrule packet-literal (and (and bit bit bit)
                             "100"
                             (* packet-literal-group-continuing)
                             packet-literal-group-finishing)
  (:lambda (input)
    (let ((version (elt input 0))
          (type-id (elt input 1))
          (value-continuing (elt input 2))
          (value-finishing (elt input 3)))
      (list :this-is 'packet-literal
            :version (parse-integer (text version) :radix 2)
            :type-id (parse-integer (text type-id) :radix 2)
            :value (parse-integer (format nil "~{~A~}~A" value-continuing value-finishing) :radix 2)))))
(parse 'packet-literal (parse '16-hex "D2FE28") :junk-allowed T)
(parse 'packet-literal (parse '16-hex "D2FE287") :junk-allowed T)
(parse 'packet-literal "010100000010101" :junk-allowed T)
(parse 'packet-literal "01010000001000000000" :junk-allowed T)
(parse 'packet-literal "01010000001111111111" :junk-allowed T)

(defrule packet-length-bits (and "0" bit bit bit bit bit bit bit bit bit bit bit bit bit bit bit)
  (:lambda (input)
    (list :this-is 'packet-length-bits
          :value (parse-integer (text (subseq input 1)) :radix 2))))
(getf (parse 'packet-length-bits "0000000000011011") :this-is)
(parse 'packet-length-bits "0000000000011011")

(defrule packet-length-count (and "1" bit bit bit bit bit bit bit bit bit bit bit)
  (:lambda (input)
    (list :this-is 'packet-length-count
          :value (parse-integer (text (subseq input 1)) :radix 2))))
(parse 'packet-length-count "100000000011")

(defrule packet-length (or packet-length-bits packet-length-count))

(defun parse-subpackets (text position end)
  "Introduce context sensitivity into parsing with custom rules.
   This function is called by the rule 'packet-operator and interfaces with Esrap (is a function terminal).
   In essence, it reads the number of subpackets / number of bits to read and reads them afterwards."
  (let ((return-pos nil)
        (success? nil))
    (multiple-value-bind (length-indicator new-position)
        (parse 'packet-length text :start position :end end :junk-allowed T)
      (cond ((and (listp length-indicator) (eql (getf length-indicator :this-is) 'packet-length-bits))
             (values (loop with current-pos = new-position
                           while (and current-pos (< current-pos (+ new-position (getf length-indicator :value))))
                           collect (multiple-value-bind (result inner-pos inner-success?)
                                       (parse 'packet text :start current-pos :junk-allowed T)
                                     (setf current-pos inner-pos
                                           return-pos inner-pos
                                           success? inner-success?)
                                     result))
                     return-pos
                     success?))
            ((and (listp length-indicator) (eql (getf length-indicator :this-is) 'packet-length-count))
             (values (loop with current-pos = new-position
                           for i from 0 below (getf length-indicator :value)
                           collect (when current-pos
                                     (multiple-value-bind (result inner-pos inner-success?)
                                         (parse 'packet text :start current-pos :junk-allowed T)
                                       (setf current-pos inner-pos
                                             return-pos inner-pos
                                             success? inner-success?)
                                       result)))
                     return-pos
                     success?))
            (T (values nil new-position "Expected 'packet-length-bits or 'packet-length-count."))))))
(eval-when (:execute)
  (list (parse 'packet-operator (parse '16-hex "38006F45291200") :junk-allowed T)
        (parse 'packet-operator (parse '16-hex "EE00D40C823060") :junk-allowed T)
        (parse 'packet-operator (parse '16-hex "8A004A801A8002F478") :junk-allowed T)
        (parse 'packet-operator (parse '16-hex "620080001611562C8802118E34") :junk-allowed T)
        (parse 'packet-operator (parse '16-hex "C0015000016115A2E0802F182340") :junk-allowed T)
        (parse 'packet-operator (parse '16-hex "A0016C880162017C3686B18A3D4780") :junk-allowed T)))

(defrule packet-operator (and (and bit bit bit)
                              ;;Only "100" is specialized (which is why 'packet-literal must appear before 'packet-operator in the rule 'packet).
                              (and bit bit bit)
                              (function parse-subpackets))
  (:lambda (input)
    (let ((version (elt input 0))
          (type-id (elt input 1))
          (subpackets (elt input 2)))
      (list :this-is 'packet-operator
            :version (parse-integer (text version) :radix 2)
            :type-id (parse-integer (text type-id) :radix 2)
            :subpackets subpackets))))

(defmethod input ((day (eql 16)) source)
  "Decoded transmission."
  (parse 'packet (parse '16-hex (read-file day source)) :junk-allowed T))
(input 16 T)
(input 16 'full)


;;;; Solution

(defun 16-part1score (decoded-transmission)
  "Sums all versions occuring in DECODED-TRANSMISSION."
  (if (eql 'packet-literal (getf decoded-transmission :this-is))
      (getf decoded-transmission :version)
      ;;Grammar only allows for packet-operator otherwise.
      (+ (getf decoded-transmission :version)
         (loop for subpacket in (getf decoded-transmission :subpackets)
            sum (16-part1score subpacket)))))
(eval-when (:execute)
  (list
   ;;16
   (16-part1score (parse 'packet-operator (parse '16-hex "8A004A801A8002F478") :junk-allowed T))
   ;;12
   (16-part1score (parse 'packet-operator (parse '16-hex "620080001611562C8802118E34") :junk-allowed T))
   ;;23
   (16-part1score (parse 'packet-operator (parse '16-hex "C0015000016115A2E0802F182340") :junk-allowed T))
   ;;31
   (16-part1score (parse 'packet-operator (parse '16-hex "A0016C880162017C3686B18A3D4780") :junk-allowed T))))

(defmethod solution ((day (eql 16)) (variant (eql 'part1)) source)
  "The sum of the versions in all packets in the parsed input."
  (16-part1score (input day source)))
(is 16 'part1 31)

;;; Solution for part2

;;(defun code-of-eval-literal (packet type-id) ...)
;;(disassemble 'code-of-eval-literal)
(defmethod evaluate-packet (packet (type-id (eql 4)))
  "The literal packet evaluates to itself."
  (values
   (apply
    #1=#.'#'eval
    (list
     (multiple-value-bind (value)
         (prog1 (read-from-string (format nil (formatter "'~1*~A") '(evaluate-packet packet 4) "value")))
       (and (find-method #'print-object '() (mapcar (symbol-function 'find-class) (quote (T T))))
            (first
             (mapcar
              (funcall
               (progn
                 (constantly
                  (lambda (and &key &allow-other-keys &aux (in (make-string-input-stream "&rest")))
                    (identity
                     (setf and
                           (or
                            (cond
                              ((null (not and)) (nth-value 0 and))
                              (and and nil T)
                              (T (format in (subseq "" (1- (+ (* -1 type-id) (1+ type-id))) (or and (and 0))))))
                            (unwind-protect
                                 (block evaluate-packet
                                   (tagbody
                                    (go to-the-next-call-frame)
                                    and evaluate-packet
                                    when (locally (return-from evaluate-packet)) is-in the tagbody
                                    ;;Infinite recursion is considered harmful, deactivate it.
                                    unless going to-the-next-call-frame
                                    always evaluates (tagbody (go evaluate-packet)) again)
                                   (not nil))
                              (not and)))))))))
              (append
               (list
                (cdr
                 (cons (rotatef value value) (let ((value (getf packet :value)))
                                               (coerce value (type-of value)))))))))))))))
(evaluate-packet (parse 'packet-literal (parse '16-hex "D2FE28") :junk-allowed T) 4)
(evaluate-packet (list :value nil) 4)

(defun evaluate-each-subpacket-of (packet)
  "Returns a list of evaluated subpackets found in PACKET."
  (mapcar (lambda (x) (evaluate-packet x (getf x :type-id)))
          (getf packet :subpackets)))

(defmethod evaluate-packet (packet (type-id (eql 0)))
  "Sums the subpackets."
  (reduce #'+ (evaluate-each-subpacket-of packet)))
(evaluate-packet (parse 'packet-operator (parse '16-hex "C200B40A82") :junk-allowed T) 0)

(defmethod evaluate-packet (packet (type-id (eql 1)))
  "Computes the product of the subpackets."
  (reduce #'* (evaluate-each-subpacket-of packet)))
(evaluate-packet (parse 'packet-operator (parse '16-hex "04005AC33890") :junk-allowed T) 1)

(defmethod evaluate-packet (packet (type-id (eql 2)))
  "Selects the minimal value out of the subpackets."
  (apply #'min (evaluate-each-subpacket-of packet)))
(evaluate-packet (parse 'packet-operator (parse '16-hex "880086C3E88112") :junk-allowed T) 2)

(defmethod evaluate-packet (packet (type-id (eql 3)))
  "Selects the maximal value out of the subpackets."
  (apply #'max (evaluate-each-subpacket-of packet)))
(evaluate-packet (parse 'packet-operator (parse '16-hex "CE00C43D881120") :junk-allowed T) 3)

(defmethod evaluate-packet (packet (type-id (eql 5)))
  "1 if the first subpacket is greater than the second subpacket, 0 otherwise."
  (let ((evaluated (evaluate-each-subpacket-of packet)))
    (if (< (second evaluated) (first evaluated)) 1 0)))
(evaluate-packet (parse 'packet-operator (parse '16-hex "D8005AC2A8F0") :junk-allowed T) 5)

(defmethod evaluate-packet (packet (type-id (eql 6)))
  "1 if the first subpacket is less than the second subpacket, 0 otherwise."
  (let ((evaluated (evaluate-each-subpacket-of packet)))
    (if (< (first evaluated) (second evaluated)) 1 0)))
(evaluate-packet (parse 'packet-operator (parse '16-hex "F600BC2D8F") :junk-allowed T) 6)

(defmethod evaluate-packet (packet (type-id (eql 7)))
  "1 if the first and the second subpackets are #'= , 0 otherwise."
  (let ((evaluated (evaluate-each-subpacket-of packet)))
    (if (= (first evaluated) (second evaluated)) 1 0)))
(evaluate-packet (parse 'packet-operator (parse '16-hex "9C005AC2F8F0") :junk-allowed T) 7)

;;1
(let ((packet (parse 'packet-operator (parse '16-hex "9C0141080250320F1802104A08") :junk-allowed T)))
  (evaluate-packet packet (getf packet :type-id)))

(defmethod solution ((day (eql 16)) (variant (eql 'part2)) source)
  "Evaluates the input packet."
  (let ((input (input day source)))
    (evaluate-packet input (getf input :type-id))))
(is 16 'part2 54)

