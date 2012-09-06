(in-package #:cl-ring-util)
(annot:enable-annot-syntax)

@export
(defun getval (m k)
  "Returns the value for key."
  (cdr (assoc k m :test #'equal)))

@export
(defun remval (m k)
  "Removes key from alist."
  (remove k m :key #'car :test #'equal))

@export
(defun putval (m k v)
  "Returns a new map that contains the mapping of key to val."
  (acons k v (remval m k)))
  
@export
(defun putval-in (m ks v)
  "Associates a value in a nested alist, where keys is a sequence of keys
 and val is the new value and returns a new nested alist. If any levels do not exist, alists will be created."
  (destructuring-bind (k &rest ks) ks
    (if ks
	(putval m k (putval-in (getval m k) ks v))
	(putval m k v))))

@export
(defun putval-cons (m k v)
  "Associate a key with a value in a map. If the key already exists in the map,
  a list of values is associated with the key."
  (putval m k
	  (let ((cur (getval m k)))
	    (if cur
		(cons v (if (consp cur) cur (list cur)))
		v))))
 
(defun merge-ms* (m1 m2)
  (loop for (k . v) in m1
     unless (getval m2 k)
     collecting `(,k . ,v) into l
     finally (return (concatenate 'list l m2))))

@export
(defun merge-ms (&rest ms)
  (reduce #'merge-ms* (rest ms) :initial-value (first ms)))
