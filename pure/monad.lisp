#+quicklisp (quote #.(ql:quickload "closer-mop"))
(defpackage :smug/pure/monad
  (:use :cl)
  (:import-from :smug/pure/interface
                #:define-interface
                #:<interface>
                #:with-interface)
  (:export
   #:<monad>
   #:result
   #:bind
   #:mlet*
     
   #:<zero-plus>
   #:zero
   #:plus 
    
   #:<monad-zero-plus>
   #:guard))
(in-package :smug/pure/monad)


(define-interface <monad> (<interface>)
  ()
  (:generic result (<interface> value))
  (:generic bind (<interface> monadic-value monadic-function)))


(defmacro mlet* (monad bindings &body body)
  (let ((interface-form (if (listp monad) monad (list monad))))    
    (destructuring-bind (monad-interface &rest args &key (with-interface t) &allow-other-keys)
        interface-form
      (if with-interface 
          `(with-interface (,monad-interface ,@args) 
             (mlet* (,(cl:first interface-form)
                      :with-interface nil
                      ,@(rest interface-form))
                  ,bindings
                 ,@body))
          (if bindings 
              (destructuring-bind ((var form) &rest rest-of-bindings)
                  bindings
                `(funcall 
                  'bind ,monad-interface ,form 
                  (lambda (,var) 
                    ,@(when (cl:string= var "_")
                            `((declare (ignorable ,var))))
                    (mlet* ,interface-form ,rest-of-bindings
                      ,@body))))
              `(cl:progn ,@body))))))

   
(define-interface <zero-plus> (<interface>)
  ()
  (:generic zero (<interface>))
  (:generic plus (<interface> 
                  interface-value-1
                  interface-value-2)))

(define-interface <monad-zero-plus> (<monad> <zero-plus>)
    ()
    (:generic guard (<interface> 
                     predicate value
                     &rest predicate-args)))


(defmethod guard ((<m> <monad-zero-plus>) 
                    predicate value
                    &rest predicate-args)
    (if (apply predicate value predicate-args)
        (result <m> value)
        (zero <m>)))
