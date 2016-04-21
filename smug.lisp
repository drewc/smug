(defpackage :smug/smug
  (:nicknames :smug)
  (:use :cl)
  (:export 
   #:.identity
   #:.fail
   #:.item
   #:.bind
   
   #:input-empty-p
   #:input-first
   #:input-rest
   #:replace-invalid
   #:run
   #:parse  
   #:.plus
   #:.or
   #:.not
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
   #:.read-line))
(in-package :smug/smug) 

(defmacro .let* (bindings &body body)
  (if bindings
      (let ((symbol (first (first bindings))))
        `(.bind ,@(cdr (first bindings))
               (lambda (,symbol)
                 ,@(when (or (string-equal (symbol-name symbol) "_")
                             (null (symbol-package symbol)))
                         `((declare (ignorable ,symbol))))
                 (.let* ,(cdr bindings)
                   ,@body))))
      `(progn ,@body)))

(defun replace-subseq (needle haystack replacement)
  (let* ((haystack (copy-seq haystack))
         (idx (search needle haystack :test 'equal)))
    (when idx
      (replace haystack replacement :start1 idx))
    haystack))

(defun replace-invalid (old new)
  (let ((replace-invalid (find-restart 'replace-invalid)))
    (when replace-invalid
      (invoke-restart replace-invalid old new))))

(defun run (parser input)
    (restart-case
      (progn
        (funcall parser input))
      (replace-invalid (old new)
         (let ((next-replace-invalid (find-restart 'replace-invalid)))
           (if (and next-replace-invalid (not (search old input)))
             (invoke-restart 'replace-invalid old new)
             (funcall parser (replace-subseq old (copy-seq input) new)))))))

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

(defun .identity (value)
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
              (.identity x)
              (.fail)))))

(defun .optional (parser)
  (.or parser (.identity nil)))

(defun .and (p1 &rest ps)
  (.let* ((result p1))
    (if ps
        (apply #'.and ps)
        (.identity result))))


(defmacro .progn (&rest parsers)
    (if (rest parsers)
        (let ((name (gensym)))
          `(.let* ((,name ,(first parsers)))
             (.progn ,@(rest parsers))))
        (first parsers)))

(defmacro .prog1 (parser &rest parsers)
  (let ((name (gensym))
        (ignore (gensym)))
    `(.let* ((,name ,parser)
             (,ignore (.progn ,@parsers)))
       (.identity ,name))))

(defmacro .prog2 (parser1 parser2 &rest parsers)
  (let ((name (gensym))
        (ignore (gensym)))
    `(.let* ((,ignore ,parser1)
             (,name ,parser2)
             (,ignore (.progn ,@parsers)))
       (.identity ,name))))

(defun .is-not (predicate &rest args)
  (.satisfies (lambda (i) 
                 (cl:not (apply predicate i args)))))

(defun .is (predicate &rest args)
  (apply #'.satisfies predicate args))

(defun .mapcar (parser)
    (.plus (.let* ((x parser)
                   (xs (.mapcar parser)))
             (.identity (cons x xs)))
           (.identity ())))

(defun .mapc (parser)
    (.plus (.let* ((_ parser)
                   (_ (.mapc parser)))
             (.identity parser))
           (.identity parser)))

(defun .make-list (size &key (initial-element (.item)))
  (if (zerop size) 
      (.identity nil)
      (.let* ((first initial-element)
              (rest (.make-list (1- size) 
                                :initial-element initial-element)))
        (.identity (list* first rest)))))               

(defun .concatenate (output-type-spec &rest parsers)
  (if (not parsers)
      (.fail)
      (.let* ((first (first parsers))
              (rest (if (rest parsers)
                        (apply 
                         #'.concatenate output-type-spec (rest parsers))
                        (.identity nil))))
        (.identity (cl:concatenate output-type-spec first rest)))))


(defun .map (result-type parser
             &key 
               (at-least 1))
  "=> a ~result-type~ of /parser/ results."
  (.let* ((list-1 (.make-list at-least :initial-element parser))
          (list-2 (funcall (if result-type #'.mapcar #'.mapc) parser)))
    (.identity (when result-type (concatenate result-type list-1 list-2)))))

(defun .char= (x)
  (.is #'cl:char= x))

(defun .digit-char-p ()
  (.is #'cl:digit-char-p))

(defun .lower-case-p ()
  (.is #'cl:lower-case-p))

(defun .upper-case-p ()
  (.is #'cl:upper-case-p))  

(defun .read-line (&optional 
                     (eof-error-p t)
                     eof-value)
  (.let* ((text (.optional 
                 (.first (.map 'list (.is-not #'char= #\Newline)))))
          (newline (.or (.char= #\Newline)
                        (.and (.not (.item)) 
                              (.identity '())))))
    (if (or text newline)
        (.identity (concatenate 'string text (when newline (string newline))))
        (if eof-error-p 
            (.fail)
            (.identity eof-value)))))
 


(defun .string= (string)
  (if (string= string "")
      (.identity nil)
      (.let* 
          ((_ (.is 'char= (aref string 0)))
           (_ (.string= (subseq string 1))))
        (.identity string))))

(defun .char-equal (char)
  (.is #'cl:char-equal char))

(defun .string-equal (string)
  (labels ((%string-equal (string)
             (.let* ((first (.char-equal (aref string 0)))
                     (rest (if (> (length string) 1)
                                  (%string-equal (subseq string 1))
                                  (.identity nil))))
               (.identity (cons first rest)))))
    (.let* ((list (%string-equal string)))
      (.identity (coerce list 'string)))))

(defun .first (parser)
  (lambda (input)
    (let ((results (run parser input)))
       (when results (list (cl:first results))))))
