#+quicklisp (quote #.(ql:quickload "closer-mop"))
(defpackage :smug/pure/interface
    (:use :cl)
    (:export  #:<interface> 
              #:define-interface
              #:with-interface))
    
(in-package :smug/pure/interface)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric interface-options (<interface>)
   (:method-combination append))

  (defgeneric expand-interface-option (<interface> option-name 
                                       &rest
                                         option-args)
    (:method (<interface> option-name &rest option-args)
      (declare (ignore option-args)) nil)
    (:method (interface-name (option-name (eql :singleton)) &rest option-args)
      (declare (ignore option-args))
      `(defvar ,interface-name
         (make-instance ',interface-name)))
    (:method (<interface> (option-name (eql :generic)) &rest option-args)
      (destructuring-bind (n args . rest) option-args
        `(defgeneric ,n ,args ,@rest))))

  (defgeneric check-interface-option (<interface> 
                                      option-name &rest option-args)
    (:method (<interface> option-name &rest option-args)
      (declare (ignore <interface> option-name option-args)) 
      t)
    (:method (<interface>
              (option-name (eql :generic)) 
              &rest option-args)
      (c2mop:compute-applicable-methods-using-classes 
       (fdefinition (first option-args))
       (list (class-of <interface>)))))


  (defmacro define-interface (name direct-superinterfaces direct-slots
                              &rest options)
    `(cl:eval-when (:compile-toplevel :load-toplevel :execute)
       (defclass ,name ,direct-superinterfaces ,direct-slots)
       ,@(loop :for (oname . args)  :in options
            :collect (apply #'expand-interface-option name oname args))
       (defmethod interface-options append ((<i> ,name))
                  '(,@options)))))

(define-interface <interface> ()
  ())

(defgeneric expand-interface-flet (<interface> body 
                                   &key package symbol-name
                                     &allow-other-keys)
  (:method ((<i> symbol) body &rest args &key &allow-other-keys)
    (apply #'expand-interface-flet (symbol-value <i>) body args))
  (:method ((<i> <interface>) body &key package (symbol-name #'symbol-name)
                                        interface-name
                          &allow-other-keys)
    (let ((interface-name (or interface-name 
                              (class-name (class-of <i>)))))
      `(flet (,@(loop :for (name . args) 
                   :in (interface-options <i>)
                   :when (eq name :generic)
                   :collect (let* ((gf-name (first args))
                                   (gf-package (symbol-package gf-name))
                                   (f-package (or package gf-package))
                                   (f-name (intern (funcall symbol-name gf-name) 
                                                   f-package)))
                              `(,f-name (&rest args)
                                        (apply ',gf-name ,interface-name args)))))
         ,@body))))

 
(defmacro with-interface ((interface &rest args)
                          &body body)
  (apply #'expand-interface-flet interface body args))
 


(defun check-interface-type (<interface>)
  (loop :for option :in (interface-options <interface>)
     :unless (apply #'check-interface-option <interface> option)
     :do (return nil)
     :finally (return t)))

(deftype interface ()
  `(satisfies check-interface-type))



;; (macroexpand-1 '(define-interface <interface> ()
;;               ()
;;               (:generic interface-options (<interface>)
;;                (:method-combination append))))
;; =>
;; (PROGN
;;  (DEFCLASS <INTERFACE> NIL NIL)
;;  (DEFGENERIC INTERFACE-OPTIONS
;;      (<INTERFACE>)
;;    (:METHOD-COMBINATION APPEND))
;;  (DEFMETHOD INTERFACE-OPTIONS APPEND ((<I> <INTERFACE>))
;;    '((:GENERIC INTERFACE-OPTIONS (<INTERFACE>) 
;;        (:METHOD-COMBINATION APPEND)))))
