;;;; cl-ring-core.lisp

(in-package #:cl-ring-util)
(annot:enable-annot-syntax)

@export
(defun redirect (url)
  "Returns a cl-ring response for an HTTP 302 redirect."
  `((:status  . 302)
    (:headers . ((:Location . ,url)))
    (:body    . "")))

@export
(defun redirect-after-post (url)
  "Returns a cl-ring response for an HTTP 303 redirect."
  `((:status  . 303)
    (:headers . ((:Location . ,url)))
    (:body    . "")))

@export
(defun not-found (body)
  "Returns a 404 'not found' response."
  `((:status  . 404)
    (:headers . nil)
    (:body    . ,body)))

@export
(defun response (body)
  "Returns a skeletal cl-ring response with the given body, status of 200, and no
  headers."
  `((:status  . 200)
    (:headers . nil)
    (:body    . ,body)))

;; TODO: File and static resource responses

(defun status (resp status)
  "Returns an updated cl-ring response with the given status."
  (putval resp :status status))

(defun header (resp name value)
  "Returns an updated Ring response with the specified header added."
  (putval-in resp `(:headers ,name) (format nil "~A" value)))

(defun content-type (resp content-type)
  "Returns an updated Ring response with the a Content-Type header corresponding
  to the given content-type."
  (header resp :Content-Type content-type))

(defun charset (resp charset)
  "Returns an updated cl-ring response with the supplied charset added to the
  Content-Type header."
  (update-in resp `(:headers :Content-Type)
    (lambda (content-type)
      (-> (or content-type "text/plain")
          (replace-all ";\\s*charset=[^;]*" "")
          (str "; charset=" charset)))))

(defun set-cookie (resp name value &rest opts)
  "Sets a cookie on the response. Requires the handler to be wrapped in the
  wrap-cookies middleware."
  (putval-in resp `(:cookies ,name) (merge-ms `((:value . ,value)) opts)))

(defun responsep (resp)
  "True if the supplied value is a valid cl-ring response."
  (and (consp resp)
       (integerp (getval resp :status))
       (listp (getval resp :headers))))

      
  
