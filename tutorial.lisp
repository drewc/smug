(defpackage :smug/tutorial
  (:use :cl)
  (:export 
   #:.run
   #:.fail
   #:.plus
   #:.result
   #:.bind
   #:.or
   #:.not
   #:input-empty-p
   #:input-first
   #:input-rest
   #:.item))
(in-package :smug/tutorial) 

(defmacro .let* (bindings &body body)
  (if bindings
      (let ((symbol (first (first bindings))))
        `(.bind ,@(cdr (first bindings))
               (lambda (,symbol)
                 ,@(when (string-equal (symbol-name symbol) "_")
                         `((declare (ignorable ,symbol))))
                 (.let* ,(cdr bindings)
                   ,@body))))
      `(progn ,@body)))

(defun .run (parser input)
  (funcall parser input))

(defun .fail ()
  (lambda (input) (declare (ignore input)) nil))

(defun .plus (first-parser second-parser)
  (lambda (input)
    (append (funcall first-parser input) (funcall second-parser input))))

(defun .result (value)
  (lambda (input)
    (list (cons value input))))

(defun .bind (parser function)
  (lambda (input)
    (loop :for (value . input) :in (.run parser input)
          :append (.run (funcall function value) input))))

(defun .or (parser &rest parsers)
  (lambda (input)
    (or (funcall parser input) 
        (when parsers 
          (funcall (apply #'.or parsers) input)))))

(defun .not (parser)
  (lambda (input)
    (let ((result (funcall parser input)))
      (if result
          nil
          (list (cons t input))))))

(defgeneric input-empty-p (input)
  (:method ((input string)) (zerop (length input))))

(defgeneric input-first (input)
  (:method ((input string)) (aref input 0)))

(defgeneric input-rest (input)
  (:method ((input string))
    (make-array (1- (length input))
                :displaced-to input
                :displaced-index-offset 1
                :element-type (array-element-type input))))
(defun .item ()
  (lambda (input)
    (unless (input-empty-p input)
      (list (cons (input-first input)
                  (input-rest input))))))



(defun .satisfies (predicate &rest args)
  (.bind (.item) 
        (lambda (x) 
          (if (apply predicate x args)
              (.result x)
              (.fail)))))

(defun .maybe (parser)
  (.or parser (.result nil)))

(defun .and (p1 &rest ps)
  (.let* ((result p1))
    (if ps
        (apply #'.and ps)
        (.result result))))


(defun .progn (&rest parsers)
  (apply #'.and parsers))

(defun .prog1 (parser &rest parsers)
  (.let* ((result parser)
          (_ (apply #'.and parsers)))
    (.result result)))

(defun .prog2 (parser1 parser2 &rest parsers)
  (.and parser1 (apply #'.prog1 parser2 parsers)))

(defun .is-not (predicate &rest args)
  (.satisfies (lambda (i) 
                 (cl:not (apply predicate i args)))))

(defun .is (predicate &rest args)
  (apply #'.satisfies predicate args))

(defun .map (result-type parser
             &key
               (at-least 1))
  "=> =LIST= of /parser/ results."
  (labels ((.zero-or-more (parser)
             (.plus 
              (.result nil)
              (.let* ((x parser)
                      (xs (.zero-or-more parser)))
                (.result (cons x xs))))))
    (if (zerop at-least)
        (.zero-or-more parser)
        (.let* ((x parser)
                (xs (.map result-type parser
                          :at-least (1- at-least))))
          (.result (coerce (cons x xs) result-type))))))

(defun .char= (x)
  (.is #'cl:char= x))

(defun .digit-char-p ()
  (.is #'cl:digit-char-p))

(defun .lower-case-p ()
  (.is #'cl:lower-case-p))

(defun .upper-case-p ()
  (.is #'cl:upper-case-p))  



(defun .string= (string)
  (if (input-empty-p string)
      (.result "")
      (.let* 
          ((_ (.is 'char= (input-first string)))
           (_ (.string= (input-rest string))))
        (.result string))))

(defun .char-equal (char)
  (.is #'char-equal char))

(defun .string-equal (string)
  (labels ((%string-equal (string)
             (if (input-empty-p string)
                 (.result nil)
                 (.let* ((first (.char-equal (input-first string)))
                         (rest (%string-equal (input-rest string))))
                   (.result (cons first rest))))))
    (.let* ((list (%string-equal string)))
      (.result (coerce list 'string)))))
