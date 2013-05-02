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

(defun find-track-num (tree test)
  "Returns first track number in cue-sheet for which
   test function evaluates to T.
   Test function receives track command and its arguments
   as its first and only argument"
  (let ((tracks (second tree)))
    (position-if #'(lambda (track)
                     (funcall test track))
                 tracks)))

(defun get-from-toplevel (tree command)
  "Get the only argument of the command from
   top-level block of parse tree"
  (cdr
   (find-if #'(lambda (tblock)
                (eq (car tblock) command))
            (car tree))))

(defun get-from-toplevel-plist (tree command prop)
  "Get prop argument of the command from
   top-level block of parse tree"
  (getf (get-from-toplevel tree command) prop))

(defun get-from-track (tree track-num command)
  "Get the only argument of the command from
   track block of parse tree"
  (cdr (find-if #'(lambda (tblock)
                    (eq (car tblock) command))
                (nth track-num (second tree)))))

(defun get-from-track-plist (tree track-num command prop)
  "Get prop argument of the command from
   track block of parse tree"
  (getf (get-from-track tree track-num command) prop))

(defun get-file-name (tree track-num)
  "Returns file where track can be found"
  (or (get-from-track-plist tree track-num :file :name)
      (get-from-toplevel-plist tree :file :name)
      (error 'cue-parser-error :message "Missing mandatory file info")))

(defun get-track-index-sec (tree track-num &key (index :start))
  "Get index in seconds for track from tree.
   INDEX can be either :PREGAP, :START or number."
  (let* ((index-num
          (cond
           ((eq index :pregap) 0)
           ((eq index :start) 1)
           ((numberp index) index)
           (t (error 'cue-parser-error :message "INDEX can be either :PREGAP :START or number"))))
         (index-entry (cdr (find-if #'(lambda (entry) (and (eq (car entry) :index)
                                                           (= index-num (getf (cdr entry) :number))))
                                    (nth track-num (second tree))))))
    (if index-entry
        (let ((min (getf index-entry :min))
              (sec (getf index-entry :sec)))
          (+ (* 60 min) sec))
      (error 'cue-parser-error :message "no index entry with such number"))))

(defmacro do-tracks ((track tree) &body body)
  (let ((tracks (gensym)))
    `(let ((,tracks (second ,tree)))
       (dolist (,track ,tracks)
         ,@body))))
