 (defpackage :drewc.org/smug/package
  (:nicknames :smug)
  (:use :drewc.org/smug/pure)
  (:shadow #:result
	   #:bind
	   #:zero
	   #:plus 
	   #:item
	   #:guard
	   #:or)
 (:export 
  #:result
  #:bind
  #:zero
  #:plus 
  #:guard
  
  #:let*
  

  #:run
  
  #:item
  #:first     
  #:maybe
  #:satisfies

  ;; Library 
  #:is
  #:not
  #:is-not
  #:or
  #:split 
  #:string
  #:every
  #:some
  #:progn
  #:prog1
  #:prog2
))
 
(in-package :drewc.org/smug/pure)

(defun drewc.org/smug/package:result (value)
  (drewc.org/smug/pure:result <parser> value))

(defun drewc.org/smug/package:item ()
  (drewc.org/smug/pure:item <parser>))

(defun drewc.org/smug/package:guard (predicate value &rest predicate-args)
  (apply #'drewc.org/smug/pure:guard <parser> 
	 predicate value predicate-args))

(defun drewc.org/smug/package:or (parser &rest parsers)
  (if parsers
      (maybe parser (apply #'drewc.org/smug/package:or parsers))
      parser))


(defmacro drewc.org/smug/package:let* (bindings 
				       &body body)
  `(mlet* (<parser> :package :drewc.org/smug/package)
       ,bindings ,@body))
