(in-package :cue-parser)

(define-condition cue-parser-error ()
  ((message :initarg :message
            :reader error-message))
  (:report (lambda (c s)
             (format s "cue-parser error: ~A"
                     (error-message c))))
  (:documentation "This condition occurs only in helper functions when
                   parsed tree does not pass sanity checks.
                   Then parsing, catch esrap-error"))
;; Basic "letters" of our alphabet
(defrule digit (or "1" "2" "3" "4" "5" "6" "7" "8" "9" "0"))

;; Alphanumeric and other allowed characters
;; FIXME: Some of these symbols are not actually allowed in file name string
;; if it is not quoted

(defun allowed-character (c)
  (or (alphanumericp c)
      (find c "/,.&-:()'")))

(defrule allowed (allowed-character character))
(defrule allowed-space (or allowed " "))

;; Number
(defrule number (+ digit)
  (:lambda (list)
           (parse-integer (text list) :radix 10)))

;; Simple or quoted string
(defrule string (or (+ allowed) (and "\"" (+ allowed-space) "\""))
  (:lambda (list)
           (if (and (= (length list) 3)
                    (equal (first list) "\""))
               (text (second list))
             (text list))))

;; Strings allowed in comments
(defrule rem-string (+ (or allowed "\"" " "))
  (:lambda (list) (text list)))

(defrule media-catalog-number (and digit digit digit digit digit digit digit digit digit digit digit digit digit )
  (:lambda (list)
           (parse-integer (text list) :radix 10)))

(defrule track-number (and digit digit)
  (:lambda (list)
           (parse-integer (text list) :radix 10)))

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
  (:destructure (catalog number)
                (declare (ignore catalog))
                (cons :catalog number)))

(defrule file-command (and "FILE " string " " filetype)
  (:destructure (file name space type)
                (declare (ignore file space))
                (list :file :name name :type type)))

(defrule flags-command (and "FLAGS " (+ (and flags (? " "))))
  (:destructure (flags list)
                (declare (ignore flags))
                (cons :flags
                      (loop for flag in list collect (car flag)))))

(defrule index-command (and "INDEX " track-number " " track-number ":" track-number ":" track-number)
  (:destructure (index track-number space min colon1 sec colon3 frame)
                (declare (ignore index space colon1 colon3 frame))
                (list :index :number track-number :min min :sec sec)))

(defrule performer-command (and "PERFORMER " string)
  (:destructure (performer string)
                (declare (ignore performer))
                (cons :performer string)))

(defrule title-command (and "TITLE " string)
  (:destructure (title string)
                (declare (ignore title))
                (cons :title string)))

(defrule track-command (and "TRACK " track-number " " datatype)
  (:destructure (track number space type)
                (declare (ignore track space))
                (list :track :number number :type type)))

;; File structure
;; As described in CUE-sheet documentation FILE command
;; must be first command in file possible with exception
;; of CATALOG. But in practice with is not always true.
;; So we will not relay on this.

;; Try to determine CUE sheet structure relaying on indents
;; or by judging FILE or TRACK command as starting command
;; of new block

;; Command which are allowed inside a block
(defrule inner-command (or title-command index-command
                           performer-command flags-command))
;; Commands which can be outside a block
(defrule toplevel-command (or rem-command catalog-command
                              flags-command performer-command
                              title-command      file-command))

(defrule inner-line (and inner-command #\NewLine)
  (:lambda (list) (first list)))

(defrule toplevel-line (and toplevel-command #\NewLine)
  (:lambda (list) (first list)))

(defrule file-line (and file-command #\NewLine)
  (:lambda (list) (first list)))

(defrule track-line (and track-command #\NewLine)
  (:lambda (list) (first list)))

(defrule inner-block (and (? file-line) track-line
                          (+ inner-line))
  (:destructure (file track inner-commands)
                (let ((inner-s-expr (cons track inner-commands)))
                  (if file (cons file inner-s-expr)
                    inner-s-expr))))

;; Final rule for whole cue sheet
(defrule cue-sheet (and (+ toplevel-line) (+ inner-block)))

(defun parse-cue (stream)
  "Parse cue-sheet from stream.
   Stream must be opened as character stream.
   User must detect and set :external-format by himself"
  (let ((data
         (loop for line = (read-line stream nil)
               while line
               collect (concatenate 'string (string-trim '(#\Tab #\Space) line)
                                    '(#\NewLine)))))
    (parse 'cue-sheet
           (apply #'concatenate 'string data))))
