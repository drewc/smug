 (defpackage :drewc.org/smug/package
  (:nicknames :smug)
  (:use :drewc.org/smug/pure)
 (:export 
  #:result
  #:bind
  #:let*
  
  #:zero
  #:plus 
  #:guard
  
  #:item
  #:first     
  #:maybe
  #:satisfies

  ;; Library 
  #:is
  #:is-not
  #:split
  #:satisfies 
  #:every
  #:some
  #:progn
  #:prog1
  #:prog2
))
 
(in-package :drewc.org/smug/pure)


(defmacro drewc.org/smug/package:let* (bindings 
				       &body body)
  `(mlet* (<parser> :package :drewc.org/smug/package)
       ,bindings ,@body))

(export 'drewc.org/smug/package:let* 
	:drewc.org/smug/package)
