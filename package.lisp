 (defpackage :drewc.org/smug/package
  (:nicknames :smug)
  (:use :drewc.org/smug/pure)
  (:shadow #:result
	   #:bind
	   #:zero
	   #:plus 
	   #:item
	   #:guard)
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


(defmacro drewc.org/smug/package:let* (bindings 
				       &body body)
  `(mlet* (<parser> :package :drewc.org/smug/package)
       ,bindings ,@body))

(export 'drewc.org/smug/package:let* 
	:drewc.org/smug/package)
