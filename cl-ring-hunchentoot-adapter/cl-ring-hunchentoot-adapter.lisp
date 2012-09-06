;;;; cl-ring-hunchentoot-adapter.lisp

(in-package #:cl-ring-hunchentoot-adapter)
(annot:enable-annot-syntax)


(defun build-response (response)
  "Create a response Hunchentoot understands"
  (setf (hunchentoot:return-code hunchentoot:*reply*) (cdr (assoc :status response)))
  (loop for (header . val) in (cdr (assoc :header response))
       do (setf (hunchentoot:header-out header hunchentoot:*reply*) val))
  (cdr (assoc :body response)))

(defun hunchentoot-handler-proxy (handler)
  "Returns an Hunchentoot handler implementation for the given cl-ring handler."
  (lambda (request)
    (let  ((request (build-request request)))
      (lambda ()
	(build-response (funcall handler request))))))

(defun build-request (request)
  "Create the request plist from the Hunchentoot request object."
  
  `( ;; required
    (:host . ,(hunchentoot:host request))
    (:request-method . ,(hunchentoot:request-method request))
    (:uri . ,(hunchentoot:request-uri request))
    (:scheme . ,(hunchentoot:header-in :x-forwarded-proto request))
    (:request-method . ,(hunchentoot:request-method request))
    (:headers . ,(hunchentoot:headers-in request))
    ;; optional
    (:query-string . ,(hunchentoot:query-string request))
    (:content-type . ,(hunchentoot:header-in :content-type request))
    (:content-length . ,(hunchentoot:header-in :content-length request))
    (:body . ,(hunchentoot:raw-post-data :request request :external-format NIL :force-text NIL :force-binary NIL :want-stream T))))

@export
(defun run-hunchentoot (handler &optional hunchentoot-options)
  "Start the Hunchentoot web server to serve the given handler according to the supplied options:

:configurator - a function called with the Hunchentoot Server instance

:port - the port to listen on (defaults to 80)

:address - The address the Hunchentoot is listening on. If address is a string 
denoting an IP address, then the server only receives connections for that address.
If address is NIL, then the server will receive connections to all IP addresses
on the machine. This is the default.

:access-log-destination - Destination of the access log which contains one log
entry per request handled in a format similar to Apache's access.log.
Can be set to a pathname or string designating the log file, to a
open output stream or to NIL to suppress logging.

For a full list of supported options please consult the Hunchentoot doc."
  (let ((s (apply #'make-instance 'hunchentoot:easy-acceptor hunchentoot-options))
	(configurator (getf hunchentoot-options :configurator)))
    (push (hunchentoot-handler-proxy handler) hunchentoot:*dispatch-table*)
    (when configurator (funcall configurator s))
    (hunchentoot:start s)))

@export
(defun stop (s)
  "Stop the Hunchentoot server instance."
  (when s
    (hunchentoot:stop s)))
