;;;; content-type.lisp

(in-package #:cl-ring-middleware)
(annot:enable-annot-syntax)

@export
(defun wrap-content-type (handler &rest opts)
  "Middleware that adds a content-type header to the response if one is not
  set by the handler. Uses the ring.util.mime-type/ext-mime-type function to
  guess the content-type from the file extension in the URI. If no
  content-type can be found, it defaults to 'application/octet-stream'.

  Accepts the following options:
    :mime-types - a map of filename extensions to mime-types that will be
                  used in addition to the ones defined in
                  ring.util.mime-types/default-mime-types"
  (lambda (req)

    (let ((resp (funcall handler req)))
    (when resp
      (if (getval-in resp `(:headers :Content-Type))
        resp
        (let ((mime-type (ext-mime-type (getval req :uri) (getval opts :mime-types))))
          (content-type resp (or mime-type "application/octet-stream"))))))))
