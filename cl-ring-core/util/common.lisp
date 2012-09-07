(in-package #:cl-ring-util)
(annot:enable-annot-syntax)

(defmacro -> (&rest args)
    "Threads the expr through the forms. Inserts x as the
      second item in the first form, making a list of it if it is not a
      list already. If there are more forms, inserts the first form as the
      second item in second form, etc."
  (cl-pattern:match args
    ((x) x)
    ((x form) (if (listp form)
        `(,(first form) ,x ,@(rest form))
        (list form x)))
    ((x form . more) `(-> (-> ,x ,form) ,@more))))
