
(defpackage :drewc.org/smug/test/monparsing/monad
 (:use :cl)
 (:shadow #:satisfies
          #:char
          #:string))

(in-package :drewc.org/smug/test/monparsing/monad)

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
                 (make-array (1- (length string))
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


(defun satisfies (predicate &rest args)
  (mlet* <parser> ((x (item)))
    (apply #'guard predicate x args)))       

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

(defun string (string)
   (if (string= string "")
       (result <parser> nil)
       (mlet* <parser> 
         ((_ (char (aref string 0)))
          (_ (string (subseq string 1))))
        (result string))))
(assert (equal '(("hello" . " there"))
               (funcall (string "hello") 
                        "hello there")))

(assert (cl:null (funcall (string "hello") 
                          "helicopter")))

(defun many (parser)
    (mlet* <parser> ()
      (plus (mlet* <parser> 
                ((x parser)
                 (xs (many parser)))
              (result (cons x xs)))
            (result nil))))

(defun many1 (parser)
  (mlet* <parser>
      ((x parser)
       (xs (many parser)))
    (result (cons x xs))))

(defun word ()
  (many (letter)))


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


(defun nat () 
  (mlet* <parser>
   ((xs (many1 (digit))))
  (result (read-from-string (coerce xs 'cl:string)))))

#+end_sr

#+name: test lisp <parser> nat
#+begin_src lisp
  (assert (equal '((124 . "") (12 . "4") (1 . "24")) 
                 (funcall (nat) "124")))  

(assert (equal '((124 . "") (12 . "4") (1 . "24")) 
               (funcall (nat) "124")))  

(defun int ()
 (mlet* <parser> ()
   (plus (mlet* <parser>
             ((_ (char #\-))
              (n (nat)))
           (result (- n)))
         (nat))))  

(assert (and (equal (funcall (int) "12345")
                    '((12345 . "") (1234 . "5") (123 . "45") 
                      (12 . "345") (1 . "2345")))
             (equal (funcall (int) "-12345")
                    '((-12345 . "") (-1234 . "5") (-123 . "45") 
                      (-12 . "345") (-1 . "2345")))
             (cl:null (funcall (int) "#-12345"))))


(defun int ()
  (mlet* <parser> 
      ((op (plus (mlet* <parser> ((_ (char #\-)))
                   (result #'-))
                 (result #'identity)))           
       (n (nat)))
    (result (funcall op n))))

(assert (and (equal (funcall (int) "12345")
                    '((12345 . "") (1234 . "5") (123 . "45") 
                      (12 . "345") (1 . "2345")))
             (equal (funcall (int) "-12345")
                    '((-12345 . "") (-1234 . "5") (-123 . "45") 
                      (-12 . "345") (-1 . "2345")))
             (cl:null (funcall (int) "#-12345"))))


(defun ints () 
  (mlet* <parser> 
      ((_ (char #\[))
       (n (int))
       (ns (many (mlet* <parser>
                     ((_ (char #\,))
                      (x (int)))
                   (result x))))
       (_ (char #\])))
    (result (cons n ns))))    
 
(assert (equal (funcall (ints) "[1,234,567]")
               '(((1 234 567) . ""))))

(defun sepby1 (parser sep)
  (mlet* <parser>
     ((x parser)
      (xs (many (mlet* <parser> 
                 ((_ sep) 
                  (y parser))
                 (result y)))))
     (result (cons x xs))))
 
(defun ints ()
  (mlet* <parser> 
      ((_ (char #\[))
       (ns (sepby1 (int) (char #\,)))
       (_ (char #\])))
    (result ns)))


(assert (equal (funcall (ints) "[1,234,567]")
               '(((1 234 567) . ""))))

(defun bracket (open-parser parser close-parser)
  (mlet* <parser>
      ((_ open-parser)
       (x parser)
       (_ close-parser))
    (result x)))

(defun ints () 
  (bracket (char #\[) (sepby1 (int) (char #\,)) (char #\])))
