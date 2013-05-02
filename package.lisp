(defpackage cue-parser
  (:nicknames :cue)
  (:use #:cl #:esrap)
  (:export #:parse-cue ;; Parser
           #:cue-parser-error ;;Condition (if something goes wrong
           #:find-track-num ;; Helper functions
           #:get-from-toplevel
           #:get-from-toplevel-plist
           #:get-from-track
           #:get-from-track-plist
           #:get-file-name
           #:get-track-index-sec))
