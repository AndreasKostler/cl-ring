(in-package #:cl-ring-util)
(annot:enable-annot-syntax)

;; The following functions are a (almost identical) carbon copy of the respective
;; hunchentoot functions. Thanks Edi!

@export
(defun url-decode (string &optional (external-format :utf8))
  "Decodes a URL-encoded STRING which is assumed to be encoded using
the external format EXTERNAL-FORMAT."
  (when (zerop (length string))
    (return-from url-decode ""))
  (let ((vector (make-array (length string) :element-type 'flexi-streams:octet :fill-pointer 0))
        (i 0)
        unicodep)
    (loop
       (unless (< i (length string))
	 (return))
       (let ((char (aref string i)))
	 (labels ((decode-hex (length)
		    (prog1
			(parse-integer string :start i :end (+ i length) :radix 16)
		      (incf i length)))
		  (push-integer (integer)
		    (vector-push integer vector))
		  (peek ()
		    (aref string i))
		  (advance ()
		    (setq char (peek))
                  (incf i)))
	   (cond
	     ((char= #\% char)
	      (advance)
	      (cond
		((char= #\u (peek))
		 (unless unicodep
		   (setq unicodep t)
		   (upgrade-vector vector '(integer 0 65535)))
		 (advance)
		 (push-integer (decode-hex 4)))
		(t
		 (push-integer (decode-hex 2)))))
	     (t
	      (push-integer (char-code (case char
					 ((#\+) #\Space)
                                      (otherwise char))))
	      (advance))))))
    (cond (unicodep
           (upgrade-vector vector 'character :converter #'code-char))
          (t (flexi-streams:octets-to-string vector :external-format (flexi-streams:make-external-format external-format))))))

@export
(defun url-encode (string &optional (external-format :utf8))
  "URL-encodes a string using the external format EXTERNAL-FORMAT."
  (with-output-to-string (s)
    (loop for c across string
       for index from 0
       do (cond ((or (char<= #\0 c #\9)
		     (char<= #\a c #\z)
		     (char<= #\A c #\Z)
		     ;; note that there's no comma in there - because of cookies
		     (find c "$-_.!*'()" :test #'char=))
		 (write-char c s))
		(t (loop for octet across (flexi-streams:string-to-octets string
							    :start index
							    :end (1+ index)
							    :external-format (flexi-streams:make-external-format external-format))
		      do (format s "%~2,'0x" octet)))))))


@export
(defun form-decode-str (string &optional (external-format :utf8))
  "Decode the supplied www-form-urlencoded string using the specified encoding,
  or UTF-8 by default."
  (url-decode string external-format))

@export
(defun form-decode (encoded &optional (external-format :utf8))
  "Decode the supplied www-form-urlencoded string using the specified encoding,
  or UTF-8 by default. If the encoded value is a string, a string is returned.
  If the encoded value is a map of parameters, a map is returned."
  (if (search "=" encoded) 
      (mapcar #'(lambda (entry)
		  (destructuring-bind (name &optional value)
		      (split "=" entry :limit 2)
		    (cons (string-trim " " (url-decode name external-format))
			  (url-decode (or value "") external-format))))
	      (split "&" encoded))
      (form-decode-str encoded external-format)))

@export
(defgeneric form-encode (x &optional external-format)
  (:documentation "Encode the supplied value into www-form-urlencoded format, often used in
  URL query strings and POST request bodies, using the specified encoding.
  If the encoding is not specified, it defaults to UTF-8"))

@export
(defmethod form-encode ((x string) &optional (external-format :utf-8))
  (url-encode x external-format))

@export
(defmethod form-encode ((x cons) &optional (external-format :utf-8))
  (labels ((encode (x) (form-encode x external-format))
	   (encode-param (k v) 
	     (str (encode (string-or-symbol-name k)) "=" (encode v))))
    (let ((strings (loop for (k . v) in x
		      append (if (listp v))))))))
				 (map 'list (lambda (x) 
					      (encode-param k x)) v)
				 `(,(encode-param k v))))))
      (join strings "&"))))

