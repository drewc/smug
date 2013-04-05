
(defpackage :drewc.org/smug/pure
  (:use :cl)
  (:import-from :drewc.org/smug/pure/interface
                 #:define-interface
                 #:<interface>)
  (:import-from :drewc.org/smug/pure/monad  
                #:<monad>
                #:result
                #:bind
                #:mlet*
     
                #:<zero-plus>
                #:zero
                #:plus 
    
                #:<monad-zero-plus>
                #:guard)
  (:shadow #:satisfies
           #:first
           #:not
           #:progn
           #:prog1
           #:prog2
           #:every
           #:some
           #:string)
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

   #:run
    
   ;; Parser Library
   #:is
   #:is-not
   #:split
   #:maybe
    
;;; Shadows CL
   #:not
   #:string
   #:first
   #:not
   #:satisfies 
   #:every
   #:some
   #:progn
   #:prog1
   #:prog2
   ))
  
(in-package :drewc.org/smug/pure)
  
    
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
 
 
(defun satisfies (predicate &rest args)
  (mlet* <parser> ((x (item)))
    (apply #'guard predicate x args)))  
 
(defun first (parser)
  (lambda (input)
    (let ((results (funcall parser input)))
       (when results (list (cl:first results))))))
 
(defun maybe (parser1 parser2)
  (first (plus <parser> parser1 parser2)))
 
(defun run (parser input)
  (let ((result (funcall parser input)))
    (values (car (cl:first result)) 
            (cdr (cl:first result))
            (rest result))))
  
 
(defun is (predicate &rest args)
  (apply #'satisfies predicate args))
  
(defun not (parser)
  (let ((true (gensym)))
    (mlet* <parser> ((true? (maybe parser (result true))))
      (guard #'eq true? true))))
   
(defun is-not (predicate &rest args)
  (satisfies (lambda (i) 
               (cl:not (apply predicate i args)))))

(defun string (string &rest args 
               &key (item-is #'char=)
                 (coerce 'cl:string))
  (if (string= string "")
      (result <parser> nil)
      (mlet* <parser> 
          ((first (is item-is (aref string 0)))
           (rest (apply 'string (subseq string 1) 
                        :coerce nil
                        args)))
        (let ((list (cons first rest)))
          (result (if coerce (coerce list coerce) list))))))

     
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
