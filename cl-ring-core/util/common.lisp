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
	    
