;; Copyright (c) 2013, Vasily Postnicov
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met: 
;;
;; 1. Redistributions of source code must retain the above copyright notice, this
;;    list of conditions and the following disclaimer. 
;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;    this list of conditions and the following disclaimer in the documentation
;;    and/or other materials provided with the distribution. 
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
;; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cue-parser)

;; FIXME: Is there more elegant solution?
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun repeat-n-times (form n)
    (cons 'and
          (loop repeat n
             collect form))))

(defun number-parser (list)
  (parse-integer (text list) :radix 10))

(defun string-to-keyword (list)
  (destructuring-bind (string val) list
    (cons (intern (subseq string 0 (1- (length string)))
                  (find-package :keyword))
          val)))

;; Basic "letters" of our alphabet
(defrule digit (digit-char-p character))
(defrule alphanumeric (alphanumericp character))

;; Alphanumeric and other allowed characters
;; FIXME: Some of these symbols are not actually allowed in file name string
;; if it is not quoted

(defun not-an-but-allowed (c)
  (find c "!#$%&'()*+,-./:;<=>?@[\]^_`"))

(defrule allowed (or (not-an-but-allowed character)
                     alphanumeric))
(defrule allowed-or-space (or allowed #\Space))

;; Number
(defrule number (+ digit)
  (:function number-parser))

;; Simple or quoted string
(defrule string (or (+ allowed) (and #\" (+ allowed-or-space) #\"))
  (:lambda (list)
           (if (and (= (length list) 3)
                    (string= (first list)
                             (string #\"))
                    (string= (third list)
                             (string #\")))
               (text (second list))
             (text list))))

;; Strings allowed in comments
(defrule rem-string (+ (or allowed #\" #\Space))
  (:function text))

;; ISRC code
(defrule isrc-code (and #.(repeat-n-times 'alphanumeric 5)
                        #.(repeat-n-times 'digit 7))
  (:function text))

(defrule media-catalog-number #.(repeat-n-times 'digit 13)
  (:function number-parser))

(defrule 2digits #.(repeat-n-times 'digit 2)
  (:function number-parser))

(defrule filetype (or "BINARY" "WAVE" "MP3" "AIFF" "MOTOROLLA"))
(defrule flags (or "DCP" "4CH" "PRE" "SCMS"))
(defrule datatype (or "AUDIO" "CDG" "MODE1/2048" "MODE1/2352"
                      "MODE2/2336" "MODE2352" "CDI/2336"
                      "CDI/2352"))

;; Commands
(defrule rem-command (and "REM" (? rem-string))
  (:destructure (rem comment)
                (declare (ignore rem))
                (cons :rem comment)))

(defrule catalog-command (and "CATALOG " media-catalog-number)
  (:function string-to-keyword))

(defrule file-command (and "FILE " string #\Space filetype #\NewLine)
  (:destructure (file name space type nl)
                (declare (ignore file space nl))
                (list :file :name name :type type)))

(defrule flags-command (and "FLAGS " (+ (and flags (? #\Space))))
  (:destructure (flags list)
                (declare (ignore flags))
                (cons :flags (mapcar #'car list))))

(defrule index-command (and "INDEX " 2digits #\Space 2digits #\: 2digits #\: 2digits)
  (:destructure (index index-number space min colon1 sec colon3 frame)
                (declare (ignore index space colon1 colon3 frame))
                (list :index :number index-number :min min :sec sec)))

(defrule performer-command (and "PERFORMER " string)
  (:function string-to-keyword))

(defrule title-command (and "TITLE " string)
  (:function string-to-keyword))

(defrule track-command (and "TRACK " 2digits #\Space datatype #\NewLine)
  (:destructure (track number space type nl)
                (declare (ignore track space nl))
                (list :track :number number :type type)))

(defrule isrc-command (and "ISRC " isrc-code)
  (:function string-to-keyword))

;; File structure
;; The parser divides CUE sheet on toplevel block, where comments, album title,
;; performer and so on are stored and inner blocks, each related to a separate
;; track. TRACK command starts a new block. There can be FILE command
;; preceding TRACK command to specify a file where TRACK is stored.

;; Command which are allowed inside a block
(defrule inner-command (and (or title-command index-command isrc-command
                                performer-command flags-command)
                            #\NewLine)
  (:function first))
;; Commands which can be outside a block
(defrule toplevel-command (and (or rem-command catalog-command
                                   flags-command performer-command
                                   title-command)
                               #\NewLine)
  (:function first))

(defrule inner-block (and (? file-command) track-command
                          (+ inner-command))
  (:destructure (file track inner-commands)
                (let ((inner-s-expr (cons track inner-commands)))
                  (if file (cons file inner-s-expr)
                    inner-s-expr))))

;; Final rule for whole cue sheet
(defrule cue-sheet (and (+ toplevel-command) (+ inner-block)))

(defun parse-cue (stream)
  "Parse CUE sheet from the @cl:param(stream). Stream must be opened as a
 character stream. User must detect and set external format by himself"
  (parse 'cue-sheet
         ;; This is ugly: read the whole file into a string
         (with-output-to-string (out)
           (loop
              for line = (read-line stream nil)
              while line do
                (write-string (string-trim '(#\Tab #\Space) line) out)
                (terpri out)))))

(defparameter *cue-external-format* '(:utf-8 :eol-style :crlf)
  "Default external format for CUE sheet files")

(defun parse-cue-file (name &optional (external-format *cue-external-format*))
  "Parses CUE sheet from file with the name
@cl:param(name). @cl:param(external-format) is used for reading from file."
  (let* ((input-raw (open name :element-type 'octet))
         (input (make-flexi-stream input-raw :external-format external-format)))
    (unwind-protect (parse-cue input) (close input))))
