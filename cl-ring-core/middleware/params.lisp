(in-package #:cl-ring-middleware)
(annot:enable-annot-syntax)

(defun parse-params (params encoding)
  (let ((params (url-encode params encoding)))
    (if (consp params) params nil)))


(defun assoc-query-params (request encoding)
  "Parse and assoc parameters from the query string with the request."
  (merge-ms-with #'merge-ms request
	      (let ((query-string (getval request :query-string)))
		(if query-string
		    (let ((params (parse-params query-string encoding)))
		      `((:query-params . ,params) (:params . ,params)))
		    `((:query-params .nil) (:params . nil))))))

(defun urlencoded-form-p (request)
  "Does a request have a urlencoded form?"
  (let ((type (getval request :Content-Type)))
    (when type
      (starts-with type "application/x-www-form-urlencoded"))))

;TODO: Merge with, slurp
(defun assoc-form-params (request encoding)
  "Parse and assoc parameters from the request body with the request."
  (merge-ms-with #'merge-ms request
	      (let ((body (and (urlencoded-form-p request) (getval request :body))))
		(if body
		    (let ((params (parse-params (slurp body :encoding encoding) encoding)))
			 `((:form-params . ,params) (:params . ,params)))
		    `((:form-params . nil) (:params . nil))))))

@export
(defun wrap-params (handler &rest opts)
  "Middleware to parse urlencoded parameters from the query string and form
  body (if the request is a urlencoded form). Adds the following keys to
  the request map:
    :query-params - a map of parameters from the query string
    :form-params  - a map of parameters from the body
    :params       - a merged map of all types of parameter
  Takes an optional configuration map. Recognized keys are:
    :encoding - encoding to use for url-decoding. If not specified, uses
                the request character encoding, or \"UTF-8\" if no request
                character encoding is set."
  
  (lambda (request)
    (let* ((encoding (or (getval opts :encoding)
			 (getval request :character-encoding)
			 (make-keyword "UTF-8")))
	   (request (if (getval request :form-params)
			request
			(assoc-form-params request encoding)))
	   (request (if (getval request :query-params)
			request
			(assoc-query-params request encoding))))
      (funcall handler request))))
