 (defpackage :smug.im/smug/package
  (:nicknames :smug)
  (:use :smug.im/smug/pure)
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
 
(in-package :smug.im/smug/pure)

(defun smug.im/smug/package:result (value)
  (smug.im/smug/pure:result <parser> value))

(defun smug.im/smug/package:item ()
  (smug.im/smug/pure:item <parser>))

(defun smug.im/smug/package:guard (predicate value &rest predicate-args)
  (apply #'smug.im/smug/pure:guard <parser> 
	 predicate value predicate-args))

(defun smug.im/smug/package:or (parser &rest parsers)
  (if parsers
      (maybe parser (apply #'smug.im/smug/package:or parsers))
      parser))

(defmacro smug.im/smug/package:let* (bindings 
				       &body body)
  `(mlet* (<parser> :package :smug.im/smug/package)
       ,bindings ,@body))
