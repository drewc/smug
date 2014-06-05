(defpackage :smug/parser/org/util
  (:use :cl :smug/pure/dot)
  (:export #:.optional
	   #:.make
	   #:.make-instance))
(in-package :smug/parser/org/util)

(defun .optional (parser)
  (.maybe parser (.result nil)))

(defun .make (function &rest plist)
  (if (and function (not plist))
      (.result (funcall function))
      (destructuring-bind (name value . rest) plist
	(.let* ((value value)
		(rest (if rest 
			  (apply #'.make nil rest)
			  (.result nil))))
	  (.result (if function 
		       (apply function name value rest)
		       (list* name value rest)))))))

(defun .make-instance (name &rest initargs)
  (apply #'.make (lambda (&rest initargs)
		   (apply #'make-instance name initargs))
	 initargs))

