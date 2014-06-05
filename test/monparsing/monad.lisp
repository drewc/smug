
(defpackage :smug/test/monparsing/monad
 (:use :cl)
 (:shadow #:satisfies
          #:char
          #:string))

(in-package :smug/test/monparsing/monad)

(eval-when (:compile-toplevel :load-toplevel :execute)

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


     

(mlet* <parser> ()
  
  (assert (equal '((1 . "asd"))
                 (funcall (result 1) "asd")))
  
  (assert (eq NIL (funcall (zero) "asd")))
  
  (assert (equal '((#\a . "sd"))
                 (funcall (item) "asd")))
  
  (assert (equal '((#\a . ""))
                 (funcall (item) "a")))
  
  (assert (null
           (funcall (item) "")))
  
  (defun char (x)
   (satisfies (lambda (y) (char= x y))))
  
  (defun digit ()
    (satisfies (lambda (x) 
                 (and (char<= #\0 x)
                      (char>= #\9 x)))))
  
  (defun lower ()
    (satisfies (lambda (x) 
                 (and (char<= #\a x)
                      (char>= #\z x)))))
  
  (defun upper ()
    (satisfies (lambda (x) 
                 (and (char<= #\A x)
                      (char>= #\Z x)))))
  

  (defun letter () (plus (lower) (upper)))
  (defun alphanum () (plus (letter) (digit)))
))











(assert (equal (funcall (word) "Yes!")
                  '(((#\Y #\e #\s) . "!") 
                    ((#\Y #\e) . "s!") 
                    ((#\Y) . "es!")
                    (NIL . "Yes!"))))


(assert (equal '(((#\a #\a #\a) . "b") 
                 ((#\a #\a) . "ab")
                 ((#\a) . "aab")
                 (NIL . "aaab"))
               (funcall (many (char #\a)) "aaab")))

(assert (equal '(((#\a #\a #\a) . "b") 
                 ((#\a #\a) . "ab") 
                 ((#\a) . "aab"))
               (funcall (many1 (char #\a)) "aaab")))  







(assert (and (equal (funcall (int) "12345")
                    '((12345 . "") (1234 . "5") (123 . "45") 
                      (12 . "345") (1 . "2345")))
             (equal (funcall (int) "-12345")
                    '((-12345 . "") (-1234 . "5") (-123 . "45") 
                      (-12 . "345") (-1 . "2345")))
             (cl:null (funcall (int) "#-12345"))))




(assert (and (equal (funcall (int) "12345")
                    '((12345 . "") (1234 . "5") (123 . "45") 
                      (12 . "345") (1 . "2345")))
             (equal (funcall (int) "-12345")
                    '((-12345 . "") (-1234 . "5") (-123 . "45") 
                      (-12 . "345") (-1 . "2345")))
             (cl:null (funcall (int) "#-12345"))))


<<ints first lisp <parser> >>
 
<<ints test lisp <parser> >>

<<sepby1 lisp <parser> >>
 


<<ints test lisp <parser> >>

<< bracket lisp <parser> >>

<< third ints lisp <parser> >>
