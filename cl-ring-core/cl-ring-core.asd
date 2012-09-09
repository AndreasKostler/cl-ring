(in-package :cl-user)

(defpackage :cl-ring-core-asd
  (:use :cl :asdf))

(in-package :cl-ring-core-asd)

(defsystem #:cl-ring-core
  :serial t
  :description "cl-ring is a common lisp web applications library inspired by Clojure's Ring"
  :author "Andreas Koestler <andreas.koestler@gmail.com>"
  :license "Public Domain"
  :depends-on (#:cl-annot #:cl-ppcre #:flexi-streams)
  :components ((:module :util
			:serial t
			:components ((:file "package")
				     (:file "codec")
				     (:file "common")
				     (:file "data")
				     (:file "mime-type")
				     (:file "response")))
	       (:module :middleware
			:serial t
			:components ((:file "package")
				     (:file "content-type")
				     (:file "params")
				     (:file "flash")))))



			 
