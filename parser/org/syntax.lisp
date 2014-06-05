(defpackage :smug/parser/org/syntax
  (:use :cl 
        :smug/parser/org/dot)
  (:export #:syntax
           #:whitespace
           #:.whitespace
           #:*whitespace*
           #:.line
           #:.until
           #:.string-of))
(in-package :smug/parser/org/syntax)

(defclass syntax () ())  

(defmethod print-object ((object syntax) stream)
  (print-unreadable-object (object stream :type t)
    (let* ((class (class-of object))
           (block (c2mop:class-slots class)))
      (pprint-logical-block (stream block)
        (loop 
           (let ((slotd (pprint-pop)))
             (pprint-indent :block 1)
             (princ (c2mop:slot-definition-name slotd) stream)
             (write-char #\Space stream)
             (format stream "~S" (handler-case 
                         (c2mop:slot-value-using-class class object slotd)
                                   (unbound-slot ()
                         "#<unbound>"))))
           (write-char #\Space stream) 
           (pprint-exit-if-list-exhausted)
           (pprint-newline :mandatory stream))))))
  
(defvar *whitespace* '(#\Space #\Tab)) 

(defun .whitespace ()
  (.is 'member *whitespace*))

(defclass whitespace (syntax)
  ((whitespace-prefix :initarg :whitespace-prefix
        :accessor whitespace-prefix)
   (whitespace-postfix :initarg :whitespace-postfix
         :accessor whitespace-postfix)))  

(defun .line (&optional (nl #\Newline))
  (.prog1 (.coerce (.some (.is-not #'char= nl)) 
                   'cl:string)
          (.is #'char= nl)))

(defstruct until 
  contents end)

(defun .until (parser &key (do (.item)) contents)
  (.let* ((until (.or parser (.result nil))))
    (if until 
        (.result (cons (reverse contents)
                       until))
        (.let* ((first do))
          (.until parser :do do :contents (list* first contents))))))  

(defun .string-of (parser &key (using '.some))
  (.coerce (funcall using parser) 'cl:string))
