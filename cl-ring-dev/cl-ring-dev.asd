(in-package :cl-user)

(defpackage :cl-ring-dev-asd
  (:use :cl :asdf))

(in-package :cl-ring-dev-asd)

(defsystem #:cl-ring-dev
  :serial t
  :description "cl-ring is a common lisp web applications library inspired by Clojure's Ring"
  :author "Andreas Koestler <andreas.koestler@gmail.com>"
  :license "Public Domain"
  :depends-on (#:cl-annot)
  :components ((:file "package")))
