(defpackage :drewc.org/smug/parser
  (:nicknames :smug/parser)
  (:use :cl)
  (:import-from :interface)
  (:import-from :drewc.org/smug/input
		 #:<input>
		 #:input-empty-p
		 #:input-first
		 #:input-rest)
  (:import-from :interface/zero-plus
		#:<zero-plus>)
  (:import-from :interface/monad/transformer/state		
		#:update)
  (:import-from :interface/monad/transformer/state		
		#:<state-transformer>)
  
  (:import-from :interface/monad/transformer/maybe		
		#:<maybe-transformer>)
  (:import-from :interface/monad/list 
		#:<list>)
  (:import-from :interface/monad
		#:mlet*
		#:bind
		#:result
		#:fail)

  (:export #:<parser>
	   #:input
	   #:item))

(in-package :drewc.org/smug/parser)

(interface:define-interface <parser> 
    (<state-transformer> <zero-plus>)
  ((input :accessor parser-input-interface 
          :initarg :input 
          :initform <input>))
  (:singleton)
  (:default-initargs :inner (<maybe-transformer> <list>))
  (:generic input (<parser>))
  (:generic (setf input) (value <parser>))
  (:generic item (<parser>)))

(defmethod input ((<p> <parser>))
  "The INPUT is simply the <STATE>s STATE."
  (update <p> #'identity))

(defmethod (setf input) (value (<p> <parser>))
  (update <p> (lambda (state) 
		(declare (ignore state)) value)))

(defmethod item ((<p> <parser>))
  (bind 
   <p> (input <p>)
   (lambda (input)
     (if (input-empty-p (parser-input-interface <p>) input)
	 (fail <p>)
	 (bind 
	  <p> (setf (input <p>) 
		    (input-rest 
		     (parser-input-interface <p>) input))
	 (constantly 
	  (result <p>
	   (input-first 
	    (parser-input-interface <p>) input))))))))
