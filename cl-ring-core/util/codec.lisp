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
  (let ((vector (make-array (length string) :element-type 'octet :fill-pointer 0))
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
          (t (octets-to-string vector :external-format (flexi-streams:make-external-format external-format))))))

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
		(t (loop for octet across (string-to-octets string
							    :start index
							    :end (1+ index)
							    :external-format (flexi-streams:make-external-format external-format))
		      do (format s "%~2,'0x" octet)))))))
