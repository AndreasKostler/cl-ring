(in-package #:cl-ring-util)

(defun getval (alist key)
  "Returns the value for key."
  (cdr (assoc key alist)))

(defun remval (alist key)
  "Removes key from alist."
  (remove key alist :key #'car))

(defun putval (alist key val)
  "Returns a new map that contains the mapping of key to val."
  (acons key val (remval alist key)))
  
 
(defun putval-in (alist keys val)
  "Associates a value in a nested alist, where keys is a sequence of keys
 and val is the new value and returns a new nested alist. If any levels do not exist, alists will be created."
  (destructuring-bind (key &rest keys) keys
    (if keys
	(putval alist key (putval-in (getval alist key) keys val))
	(putval alist key val))))

(defun putval-cons (alist key val)
  "Associate a key with a value in a map. If the key already exists in the map,
  a list of values is associated with the key."
  (putval alist key
	     (let ((cur (getval alist key)))
	       (if cur
		   (cons val (if (consp cur) cur (list cur)))
		   val))))

