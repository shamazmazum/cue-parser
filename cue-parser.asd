(defsystem :cue-parser
  :name :cue-parser
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum at gmail dot com>"
  :components ((:file "package")
	       (:file "parser" :depends-on ("package"))
               (:file "helper-functions" :depends-on ("package")))
  :depends-on (:esrap))
