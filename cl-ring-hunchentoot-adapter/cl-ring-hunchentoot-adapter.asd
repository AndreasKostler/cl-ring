;;;; cl-ring-hunchentoot-adapter.asd

(in-package :cl-user)

(defpackage :cl-ring-hunchentoot-adapter-asd
  (:use :cl :asdf))

(in-package :cl-ring-hunchentoot-adapter-asd)

(defsystem #:cl-ring-hunchentoot-adapter
  :serial t
  :description "cl-ring is a common lisp web applications library inspired by Clojure's Ring"
  :author "Andreas Koestler <andreas.koestler@gmail.com>"
  :license "Public Domain"
  :depends-on (#:hunchentoot #:cl-annot)
  :components ((:file "package")
	       (:file "cl-ring-hunchentoot-adapter")))
