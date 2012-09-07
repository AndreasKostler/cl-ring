(in-package :cl-user)

(defpackage :cl-ring-core-asd
  (:use :cl :asdf))

(in-package :cl-ring-core-asd)

(defsystem #:cl-ring-core
  :serial t
  :description "cl-ring is a common lisp web applications library inspired by Clojure's Ring"
  :author "Andreas Koestler <andreas.koestler@gmail.com>"
  :license "Public Domain"
  :depends-on (#:cl-annot #:cl-ppcre)
  :components ((:module :util
			:components ((:file "package")
				     (:file "common")
				     (:file "data")
				     (:file "mime-type")
				     (:file "response")))))
			 
