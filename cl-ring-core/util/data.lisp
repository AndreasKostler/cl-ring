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
  "Associates a value in a nested map, where keys is a sequence of keys
 and val is the new value and returns a new nested map. If any levels do not exist, maps will be created."
  (destructuring-bind (k &rest ks) ks
    (if ks
	(putval m k (putval-in (getval m k) ks v))
	(putval m k v))))

@export
(defun getval-in (m ks)
  "Returns the value in a nested map, where keys is a sequence of keys"
  (destructuring-bind (k &rest ks) ks
    (if ks
	(getval-in (getval m k) ks)
	(getval m k))))

@export 
(defun update-in (m ks fn)
  "Applies a function to the value in a nested map, where keys is a sequence of keys
and fn is the function to be applied and returns a new nested map. If any levels do not exist, maps will be created."
  (destructuring-bind (k &rest ks) ks
    (if ks
	(putval m k (update-in (getval m k) ks fn))
	(putval m k (funcall fn (getval m k))))))


@export
(defun putval-cons (m k v)
  "Associate a key with a value in a map. If the key already exists in the map,
  a list of values is associated with the key."
  (putval m k
	  (let ((cur (getval m k)))
	    (if cur
		(cons v (if (consp cur) cur (list cur)))
		v))))

(defun merge-2-ms (m1 m2)
  (loop for (k . v) in m1
     unless (getval m2 k)
     collecting `(,k . ,v) into l
     finally (return (concatenate 'list l m2))))

@export
(defun merge-ms (&rest ms)
  "Merges the maps. If there are duplicate keys, the values of the last (left-to-right)
take precedence"
  (reduce #'merge-2-ms (rest ms) :initial-value (first ms)))

@export 
(defun key (m)
  (car m))

@export
(defun val (m)
  (cdr m))

@export
(defun merge-ms-with (fn &rest maps)
  "Returns a map that consists of the rest of the maps consed onto
  the first.  If a key occurs in more than one map, the mapping(s)
  from the latter (left-to-right) will be combined with the mapping in
  the result by calling (fn val-in-result val-in-latter)."
  (labels ((merge-entry (m e)
	     (let ((k (key e))
		   (v (val e)))
	       (if (getval m k)
		   (putval m k (funcall fn (getval m k) v))
		   (putval m k v))))
	   (merge2 (map1 map2)
	     (reduce #'merge-entry map2 :initial-value map1)))
    (when (some #'identity maps)
      (reduce #'merge2 maps))))

