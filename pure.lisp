#+quicklisp (quote #.(ql:quickload "closer-mop"))

(defpackage :drewc.org/smug/pure
  (:use :cl)
  (:shadow #:satisfies
           #:first
           #:progn
           #:prog1
           #:prog2
           #:every
           #:some)
  (:export 
  
   #:<monad>
   #:result
   #:bind
   #:mlet*
    
   #:<zero-plus>
   #:zero
   #:plus 
   
   #:<monad-zero-plus>
   #:guard
   
   
   #:<parser>     
   #:item     
   
   ;; Parser Library
   #:is
   #:is-not
   #:split
   #:maybe
   
   ;;; Shadows CL
   #:first
   #:satisfies 
   #:every
   #:some
   #:progn
   #:prog1
   #:prog2
   ))
 
(in-package :drewc.org/smug/pure)
 
(eval-when (:compile-toplevel :load-toplevel :execute)
 (flet ((first (list) (cl:first list)))
 
  (defgeneric expand-interface-option (<interface> option-name &rest option-args)
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
              (option-name (eql :generic)) &rest option-args)
       (c2mop:compute-applicable-methods-using-classes 
               (fdefinition (first option-args))
               (list (class-of <interface>)))))
  
  (defgeneric interface-options (<interface>)
     (:method-combination append))
  
  (defmacro define-interface (name direct-superinterfaces direct-slots
                              &rest options)
    `(cl:progn  
       (defclass ,name ,direct-superinterfaces ,direct-slots)
       ,@(loop :for (oname . args)  :in options
            :collect (apply #'expand-interface-option name oname args))
       (defmethod interface-options append ((<i> ,name))
                  '(,@options))))
  
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
    
  
 
  
  (define-interface <monad> (<interface>)
    ()
    (:generic result (<interface> value))
    (:generic bind (<interface> monadic-value monadic-function)))
  
  
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
 
  (define-interface <parser> (<monad-zero-plus>) 
    ()
    (:singleton)
    (:generic item (<parser>)))
  
  (defgeneric item-input (<parser> input)
    (:method ((<p> <parser>) (input cl:null))
      input)
    (:method ((<p> <parser>) (input cl:string))
      (unless (string= input "")
        (list 
         (cons (aref input 0) 
               (multiple-value-bind (array displaced-index-offset) 
                   (array-displacement input) 
                 (let ((string (or array input))
                       (index (if array (1+ displaced-index-offset) 1)))
                   (make-array (1- (length input))
                               :displaced-to string
                               :displaced-index-offset index
                               :element-type (array-element-type string)))))))))
  
  (defmethod item ((<p> <parser>))
    (lambda (input)
      (item-input <p> input)))
 
  (defmethod result ((<p> <parser>) value)
    (lambda (input) (list (cons value input))))
  
  (defmethod bind ((<p> <parser>) parser function)
    (lambda (input)
      (loop :for (value . input) 
         :in (funcall parser input)
         :append (funcall (funcall function value) input))))
 
  (defmethod zero ((<p> <parser>))
     (constantly NIL))
  
  (defmethod plus ((<p> <parser>) parser qarser)
    (lambda (input)
      (append (funcall parser input) (funcall qarser input))))
 
  (defmacro mlet* (monad bindings &body body)
    (let ((interface-form (if (listp monad) monad (list monad))))    
      (destructuring-bind (monad-interface &rest args &key (with-interface t) &allow-other-keys)
          interface-form
        (if with-interface 
            `(with-interface (,monad-interface ,@args) 
               (mlet* (,(first interface-form)
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
                      ,@(when (string= var "_")
                              `((declare (ignorable ,var))))
                      (mlet* ,interface-form ,rest-of-bindings
                        ,@body))))
                `(cl:progn ,@body))))))
   ))


(defun satisfies (predicate &rest args)
  (mlet* <parser> ((x (item)))
    (apply #'guard predicate x args)))  

(defun first (parser)
  (lambda (input)
    (let ((results (funcall parser input)))
       (when results (list (cl:first results))))))

(defun maybe (parser1 parser2)
  (first (plus <parser> parser1 parser2)))

(defun is (predicate &rest args)
  (apply #'satisfies predicate args))
  
(defun is-not (predicate &rest args)
  (satisfies (lambda (i) 
               (not (apply predicate i args)))))
    
(defun some (parser)
  (mlet* <parser> ()
    (maybe (mlet* <parser> 
              ((x parser)
               (xs (some parser)))
            (result (cons x xs)))
          (result nil))))
 
(defun every (parser)
  (mlet* <parser>
      ((x parser)
       (xs (some parser)))
    (result (cons x xs))))
 
(defun progn (parser &rest parsers)
  (if parsers 
      (mlet* <parser> ((_ parser))
        (apply #'progn parsers))
      parser))
   
(defun prog1 (parser &rest parsers)
  (mlet* <parser> ((parser1 parser)
                   (_ (if parsers 
                          (apply #'progn parsers)
                          (result t))))
    (result parser1)))

(defun prog2 (parser1 parser2 &rest parsers)
  (mlet* <parser> ()
    (progn parser1 (apply #'prog1 parser2 parsers))))

(defun split (parser &key (by (is #'char= #\space))
                       (using #'some))
  (mlet* <parser> 
      ((x parser)
       (xs (funcall using 
             (progn by parser))))
    (result (cons x xs))))