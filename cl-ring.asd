;;;; cl-ring.asd

(in-package :cl-user)

(defpackage :cl-ring-asd
  (:use :cl :asdf))

(in-package :cl-ring-asd)

(defsystem #:cl-ring
  :serial t
  :description "cl-ring is a common lisp web applications library inspired by Clojure's Ring"
  :author "Andreas Koestler <andreas.koestler@gmail.com>"
  :license "Public Domain"
  :depends-on (#:cl-ring-core #:cl-ring-dev #:cl-ring-hunchentoot-adapter)
  :components ((:file "package")))

				     

