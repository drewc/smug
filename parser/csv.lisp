(cl:defpackage :drewc.org/smug/parser/csv
  (:shadowing-import-from :drewc.org/smug/pure-smug
			  #:every
			  #:some
			  #:progn
			  #:prog1
			  #:prog2)
   (:use :cl :drewc.org/smug/pure-smug))
  
(in-package :drewc.org/smug/parser/csv)

(defun value-character (&key (separator #\,))
  (is-not (lambda (i) 
	    (or (char= i separator)
		(char= i #\Newline)))))

(defun value (&key (separator #\,))
  (mlet* <parser> ((v (some (value-character
			     :separator separator))))
    (result (coerce v 'cl:string))))

(defun line (&key (separator #\,))
  (mlet* <parser>
      ((v (value :separator separator))
       (vs (some 
		(progn 
		  (is #'char= separator)
		  (value :separator separator)))))
    (result (cons v vs))))

(defun file (&key (separator #\,))
  (mlet* <parser> 
      ((l (line :separator separator))
       (ls (some (progn (is #'char= #\Newline)
			(line)))))
    (result (cons l ls))))
