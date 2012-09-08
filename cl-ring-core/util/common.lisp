(in-package #:cl-ring-util)
(annot:enable-annot-syntax)

@export
(defmacro -> (&rest args)
  "Threads the expr through the forms. Inserts x as the
      second item in the first form, making a list of it if it is not a
      list already. If there are more forms, inserts the first form as the
      second item in second form, etc."
  (let ((n (length args)))
    (cond ((= n 2)
	   (destructuring-bind (x form) args
	     (if (listp form)
		 `(,(first form) ,x ,@(rest form))
		 (list form x))))
	  ((> n 2)
	   (destructuring-bind (x form &rest more) args
	     `(-> (-> ,x ,form) ,@more)))
	  (T
	   args))))

@export
(defmacro ->> (&rest args)
  "Threads the expr through the forms. Inserts x as the
      last item in the first form, making a list of it if it is not a
      list already. If there are more forms, inserts the first form as the
      last item in second form, etc."
  (let ((n (length args)))
    (cond ((= n 2)
	   (destructuring-bind (x form) args
	     (if (listp form)
		 `(,(first form) ,@(rest form) ,x)
		 `(,form ,x))))
	  ((> n 2)
	   (destructuring-bind (x form &rest more) args
	     `(->> (->> ,x ,form) ,@more)))
	  (T 
	   args))))
	    
@export
(defun replace-all (s match replacement &key (preserve-case T))
  "Replaces all instance of match with replacement in s. 
match/replacement can be:
string / string 
char / char
pattern - if regex-p flag is set. 
See also replace+."
  (cl-ppcre:regex-replace-all match s replacement :preserve-case preserve-case))


@export
(defun replace+ (s match replacement &key (preserve-case T))
 "Replaces the first instance of match with replacement in s.
match/replacement can be:
char / char
string / string
pattern - if regex-p flag is set.
See also replace-all"
  (cl-ppcre:regex-replace match s replacement :preserve-case preserve-case))

@export 
(defun str (&rest strings)
  (if (> (length strings) 1)
      (reduce (lambda (acc s)
		(concatenate 'string acc s))
	      (mapcar (lambda (x) (format nil "~A" x)) (rest strings))
	      :initial-value (first strings))
      (car strings)))

@export
(defun split (regex string &key (limit nil))
  (cl-ppcre:split regex string :limit limit))

@export
(defun join (strings &optional (sep " "))
  "Concatenates a list of strings
and puts spaces between the elements."
    (format nil (format nil "~~{~~A~~^~A~~}" sep) strings)) 

@export
(defun starts-with (string prefix)
  (and (>= (length string) (length prefix))
       (string= (subseq string 0 (length prefix)) prefix)))

@export
(defun make-keyword (name) 
  (values (intern (string-upcase name) "KEYWORD")))

