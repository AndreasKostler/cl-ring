(in-package #:cl-ring-middleware)
(annot:enable-annot-syntax)

(defun wrap-flash (handler)
  "If a :flash key is set on the response by the handler, a :flash key with
  the same value will be set on the next request that shares the same session.
  This is useful for small messages that persist across redirects."
  (lambda (request)
    (let* ((session (getval request :session))
	   (flash (getval  session :_flash))
	   (session (remval session :_flash))
	   (request (-> request
			(putval :session session)
			(putval :flash flash))))
      (let ((response (funcall handler request)))
	(when response
	  (let* ((session (or (getval response :session) session))
		 (session (let ((flash (getval response :flash)))
			    (if flash
				(putval (funcall response :session session) :_flash flash)
				session))))
	    (if (or flash (getval response :flash) (getval response :session))
		(putval response :session session)
		response)))))))
