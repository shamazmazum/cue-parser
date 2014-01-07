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
  "Returns first track in cue-sheet which satisfies the test.
   Test function receives list of track command and its
   arguments as its first and only argument."
  (find-if test (second tree)))

(defun get-from-toplevel (tree command)
  "Get the only argument of the command from
   top-level block of parse tree"
  (flet ((needed-commandp (toplevel-command)
           (eq (car toplevel-command) command)))
    (cdr
     (find-if #'needed-commandp (car tree)))))

(defun get-from-toplevel-plist (tree command prop)
  "Get prop argument of the command from
   top-level block of parse tree"
  (getf (get-from-toplevel tree command) prop))

(defun get-from-track (track command)
  "Get the only argument of the command from
   track block of parse tree"
  (flet ((needed-commandp (track-command)
           (eq (car track-command) command)))
    (cdr (find-if #'needed-commandp track))))

(defun get-from-track-plist (track command prop)
  "Get prop argument of the command from
   track block of parse tree"
  (getf (get-from-track track command) prop))

;; Ugly
(defun get-file-name (tree track)
  "Returns file where track can be found"
  (let* ((tracks (second tree))
         (track-with-file
          (find-if #'(lambda (track%)
                       (get-from-track track% :file))
                   tracks
                   :start 0
                   :end (1+ (position track tracks :test #'equalp))
                   :from-end t)))

    (get-from-track-plist track-with-file :file :name)))

(defun get-track-index-sec (track &optional (index :start))
  "Get index in seconds for track from tree.
   INDEX can be either :PREGAP, :START or number."
  (declare (type (or number (member :pregap :start)) index))
  (let* ((index-num
          (cond
           ((eq index :pregap) 0)
           ((eq index :start) 1)
           (t index)))
         (index-entry (cdr (find-if #'(lambda (entry) (and (eq (car entry) :index)
                                                           (= index-num (getf (cdr entry) :number))))
                                    track))))
    
    (if index-entry
        (let ((min (getf index-entry :min))
              (sec (getf index-entry :sec)))
          (+ (* 60 min) sec)))))

(defmacro do-tracks ((track tree) &body body)
  (let ((tracks (gensym)))
    `(let ((,tracks (second ,tree)))
       (dolist (,track ,tracks)
         ,@body))))
