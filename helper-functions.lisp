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

(defun find-track (tree test)
  "Returns first track in parsed CUE sheet @cl:param(tree) which satisfies the
 test. Test function receives a track as its only argument."
  (find-if test (second tree)))

(defun get-track-by-idx (tree idx)
  "Return track from parsed CUE sheet @cl:param(tree) by its index
 cl:param(idx)."
  (nth idx (second tree)))

(defun get-toplevel (tree)
  "Get toplevel block from parsed CUE sheet. Just wrapper around @c(first)."
  (first tree))

(defun get-tracks (tree)
  "Get tracks from parsed CUE sheet. Just wrapper around @c(second)."
  (second tree))

(defun get-command-arg (tree command)
  "Get the argument of the command from CUE sheet block
 @cl:param(tree). @cl:param(tree) can be the toplevel block or a track block."
  (flet ((needed-commandp (toplevel-command)
           (eq (car toplevel-command) command)))
    (cdr (find-if #'needed-commandp tree))))

(defun get-command-named-arg (tree command arg-name)
  "Get the named argument of the command from CUE sheet block
 @cl:param(tree). @cl:param(tree) can be the toplevel block or a track block."
  (getf (get-command-arg tree command) arg-name))

;; Ugly
(defun get-file-name (tree track)
  "Returns a file name where the track @cl:param(track) can be found"
  (let* ((tracks (second tree))
         (track-with-file
          (find-if #'(lambda (track%)
                       (get-command-arg track% :file))
                   tracks
                   :start 0
                   :end (1+ (position track tracks :test #'equalp))
                   :from-end t)))

    (get-command-named-arg track-with-file :file :name)))

(defun get-track-index-sec (track &optional (index :start))
  "Get index in seconds for a @cl:param(track). @cl:param(index) can be either
 @c(:PREGAP), @c(:START) or a number."
  (declare (type (or number (member :pregap :start)) index))
  (let* ((index-num
          (cond
           ((eq index :pregap) 0)
           ((eq index :start) 1)
           (t index)))
         (index-entry (cdr
                       (find-if
                        (lambda (command)
                          (and (eq (car command) :index)
                               (= index-num (getf (cdr command) :number))))
                        track))))
    (if index-entry
        (let ((min (getf index-entry :min))
              (sec (getf index-entry :sec)))
          (+ (* 60 min) sec)))))
