(defpackage :smug/tutorial
  (:use :cl)
  (:export 
   #:.let*
   #:.map
   #:.concatenate
   #:.is
   #:.is-not
   #:.char=
   #:.char-equal
   #:.string-equal
   #:.string=
   #:.progn
   #:.prog1
   #:.prog2
   #:.and
   #:.or
   #:.not
   #:.first
   #:.optional
   #:run
   #:parse
   #:run
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

(defun run (parser input)
  (funcall parser input))
(defun parse (parser input)
  (let ((result (run parser input)))
    (when result 
      (destructuring-bind ((result . input) &rest rest)
          result      
        (apply #'values result input rest)))))

(defun .run (&rest baz)
  (error ".RUN is now called RUN ~{~A~%~}" baz))

(defun parse (parser input)
  (let ((result (run parser input)))
    (when result 
      (destructuring-bind ((result . input) &rest rest)
          result      
        (apply #'values result input rest)))))

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
    (loop :for (value . input) :in (run parser input)
          :append (run (funcall function value) input))))

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
    (multiple-value-bind (string displacement) 
        (array-displacement input)      
      (make-array (1- (length input))
                  :displaced-to (or string input)
                  :displaced-index-offset (1+ displacement)
                  :element-type (array-element-type input)))))
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

(defun .optional (parser)
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

(defun .mapcar (parser)
    (.plus (.let* ((x parser)
                   (xs (.mapcar parser)))
             (.result (cons x xs)))
           (.result ())))

(defun .mapc (parser)
    (.plus (.let* ((_ parser)
                   (_ (.mapc parser)))
             (.result parser))
           (.result parser)))

(defun .make-list (size &key (initial-element (.item)))
  (if (zerop size) 
      (.result nil)
      (.let* ((first initial-element)
              (rest (.make-list (1- size) 
                                :initial-element initial-element)))
        (.result (list* first rest)))))               

(defun .concatenate (output-type-spec &rest parsers)
  (if (not parsers)
      (.fail)
      (.let* ((first (first parsers))
              (rest (if (rest parsers)
                        (apply 
                         #'.concatenate output-type-spec (rest parsers))
                        (.result nil))))
        (.result (cl:concatenate output-type-spec first rest)))))

(defun .map (result-type parser
             &key 
               (at-least 1))
  "=> a =result-type= of /parser/ results."
  (.let* ((list-1 (.make-list at-least :initial-element parser))
          (list-2 (funcall (if result-type #'.mapcar #'.mapc) parser)))
    (.result (when result-type (concatenate result-type list-1 list-2)))))

(defun .char= (x)
  (.is #'cl:char= x))

(defun .digit-char-p ()
  (.is #'cl:digit-char-p))

(defun .lower-case-p ()
  (.is #'cl:lower-case-p))

(defun .upper-case-p ()
  (.is #'cl:upper-case-p))  

(defun .string= (string)
  (if (string= string "")
      (.result nil)
      (.let* 
          ((_ (.is 'char= (aref string 0)))
           (_ (.string= (subseq string 1))))
        (.result string))))

(defun .char-equal (char)
  (.is #'cl:char-equal char))

(defun .string-equal (string)
  (labels ((%string-equal (string)
             (.let* ((first (.char-equal (aref string 0)))
                     (rest (if (> (length string) 1)
                                  (%string-equal (subseq string 1))
                                  (.result nil))))
               (.result (cons first rest)))))
    (.let* ((list (%string-equal string)))
      (.result (coerce list 'string)))))

(defun .first (parser)
  (lambda (input)
    (let ((results (run parser input)))
       (when results (list (cl:first results))))))
