(defmacro mlet* (interface bindings &body body)
  (declare (ignorable interface))
  )

(defun many (parser)
  (plus (mlet* <parser>
	    ((x parser)
	     (xs (many parser)))
	  (result (cons x xs)))
	(result nil)))

(defun guard (predicate value 
	      &rest predicate-args)
  (if (apply predicate value predicate-args)
      (result value)
      (zero)))

(defun many1 (parser)
  (mlet* <parser>
      ((x parser)
       (xs (many parser)))
    (result (cons x xs))))


(with-open-file  (f "/etc/passwd" :external-format :latin-1)
  (labels ((field ()
	     (many (mlet* <parser> 
		       ((f (item)))
		     (guard (lambda (char) 
			      (not (char= #\: char))) f))))
	   (sep ()
	     (mlet* <parser> 
		 ((f (field))
		  (_ (char #\:)))
	       (plus (char #)))))
    
  
    (funcall (parser-let* ((fiel)))
	     
	     (read-line f nil :eof))))
