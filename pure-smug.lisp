#+quicklisp (quote #.(ql:quickload "closer-mop"))

(defpackage :drewc.org/smug/pure-smug
  (:use :cl)
  (:shadow #:satisfies
           #:progn
           #:prog1
           #:prog2
           #:every
           #:some
           #:char
           #:string)
  (:export 
   #:mlet*
   #:<parser>
   #:result
   #:bind
   #:item
   #:zero
   #:guard
   #:is
   #:is-not
   ;; Shadows CL
   #:every
   #:some
   #:progn
   #:prog1
   #:prog2
   ))
 
(in-package :drewc.org/smug/pure-smug)
 
(eval-when (:compile-toplevel :load-toplevel :execute)
 
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
  
  (defmacro define-interface (name direct-superinterfaces direct-slots
                              &rest options)
    `(cl:progn  
       (defclass ,name ,direct-superinterfaces ,direct-slots)
       ,@(loop :for (oname . args)  :in options
            :collect (apply #'expand-interface-option name oname args))
       (defmethod interface-options append ((<i> ,name))
                  '(,@options))))
  
  (defun check-interface-type (<interface>)
    (loop :for option :in (interface-options <interface>)
       :unless (apply #'check-interface-option <interface> option)
       :do (return nil)
       :finally (return t)))
  
  (deftype interface ()
    `(satisfies check-interface-type))
  
  (define-interface <interface> ()
    ()
    (:generic interface-options (<interface>)
              (:method-combination append)))
  
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
                   (make-array (1- (length string))
                               :displaced-to string
                               :displaced-index-offset index
                               :element-type (array-element-type array)))))))))
  
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

  (defmethod plus ((<p> <parser>) parser qarser)
      (lambda (input)
        (let ((list (funcall parser input)))     
          (append list (unless list (funcall qarser input))))))
 
  (defmacro mlet* (monad bindings &body body)
    (if monad     
        (let ((m (gensym)))
          `(let ((,m ,monad))
             (flet ,(loop :for (name . args) 
                       :in (interface-options (symbol-value monad))
                       :when (and (eq name :generic)
                                  (not (eq (first args) 
                                           'interface-options)))
                       :collect `(,(first args) (&rest args)
                                   (apply ',(first args) ,m args)))
               (mlet* nil ,bindings ,@body))))
        (if bindings 
            (destructuring-bind ((var form) &rest bindings)
                bindings
              `(bind ,form (lambda (,var) 
                             ,@(when (string= var "_")
                                    `((declare (ignorable ,var))))
      
                             (mlet* nil ,bindings
                                    ,@body))))
            `(cl:progn ,@body))))   )


(defun satisfies (predicate &rest args)
  (mlet* <parser> ((x (item)))
    (apply #'guard predicate x args)))  
 
(defun is (predicate &rest args)
  (apply #'satisfies predicate args))
  
(defun is-not (predicate &rest args)
  (satisfies (lambda (i) 
               (not (apply predicate i args)))))

(defun sepby1 (parser sep)
  (mlet* <parser>
     ((x parser)
      (xs (many (mlet* <parser> 
                 ((_ sep) 
                  (y parser))
                 (result y)))))
     (result (cons x xs))))
(defun split (parser &key (by this-parser))
  (sepby1 parser this-parser))
 
(defun some (parser)
  (mlet* <parser> ()
    (plus (mlet* <parser> 
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
