;;;; cl-ring-hunchentoot-adapter.asd

(asdf:defsystem #:cl-ring-hunchentoot-adapter
  :serial t
  :description "cl-ring adapter for the Hunchentoot web server."
  :author "Andreas Koestler <andreas.koestler@gmail.com>"
  :license "Public Domain"
  :depends-on (#:hunchentoot #:cl-annot)
  :components ((:file "package")
               (:file "cl-ring-hunchentoot-adapter")))

