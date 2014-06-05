(defpackage :drewc.org/smug/test/smug
  (:nicknames :smug/test/smug)
  (:use :cl)
  (:import-from :interface)
  (:import-from :drewc.org/smug/parser
		#:<parser>
		#:input
		#:item)
  (:import-from :interface/monad)
  (:import-from :interface/run
		#:run)

  (:export))


(in-package :drewc.org/smug/test/smug)

(defun test-result ()
  (let ((parsers 
	 (list (interface/monad:result 
		<parser> 1)
	       (interface/monad:mlet* 
		   <parser> ()
		 (interface/monad:result 1)))))
    (loop for parser in parsers
	 :do 
	 (assert (equal 
		  '((1 . "asd"))
		  (run <parser> parser "asd"))))))

(defun test-input-string ()
  (assert 
   (equal '(("asd" . "asd"))
	   (run <parser> 
		(input <parser>)
		"asd"))))

(defun test-item (&optional 
		    (first-char #\A) 
		    (parsee "ASD"))
  (assert 
   (eql first-char
	(caar (run <parser> (item <parser>) parsee)))))

(progn ;; Now the tests run themselves!
  (test-result)
  (test-input-string)
  (test-item))






