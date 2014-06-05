(defpackage :smug/parser/org/parameters
  (:use :cl )
  (:export #:org-parameter
	   #:define-org-parameter))
(in-package :smug/parser/org/parameters)

(defvar *org-parameters* (list))

(defun org-parameter (name)
  (let ((value (assoc name *org-parameters* :key #'car)))
    (when value 
      (destructuring-bind ((name . documentation) . value)
	  value
	(values value documentation name)))))

(defun (setf org-parameter) (value name &optional documentation)
  (setf *org-parameters* (acons (cons name documentation) value *org-parameters*)))

(defmacro define-org-parameter (name value &optional documentation)
  `(setf (org-parameter ',name ,documentation) ,value))
  



