(defpackage :drewc.org/smug/library
  (:nicknames :smug/library)
  (:use :cl)
  (:import-from :interface)
  (:import-from :interface/monad
		#:mlet*
		#:bind
		#:result
		#:fail)
  (:import-from :drewc.org/smug/parser
		#:<parser>
		#:input
		#:item)

  (:export #:=let*
	   #:=satisfies))

(in-package :drewc.org/smug/library)

(interface:define-interface <parser-library> 
   (<parser>)
  ()
  (:singleton)
  (:generic =satisfies (<parser> predicate)))

(defmacro =let* (bindings &body body)
  `(mlet* <parser-library> ,bindings ,@body))

(defmethod =satisfies ((<p> <parser>) predicate)
  (bind <p> (item <p>)
	(lambda (v) 
	  (if (funcall predicate v)
	      (result <p> v)
	      (fail <p>)))))
