(in-package :smug)

(defun result (value)
  (declare (optimize (speed 3)))
  (lambda (input)
    (list (cons value input))))

(defun fail (&key (error nil))
  (if error 
      (lambda (input)
	(declare (ignore input)) 
	(error error))
      (constantly nil)))

(defun item ()
  (lambda (input)
    (unless (input-empty-p input)
      (list (cons (input-first input)
		  (input-rest input))))))

(defun bind (parser function)
  (lambda (input)
    (loop :for (value . input) :in (funcall parser input)
          :append (funcall (funcall function value) input))))

(defun run (parser input &key (result #'caar))
  (funcall result (funcall parser input)))

(defun =satisfies (predicate)
  (bind (item) 
	(lambda (x) 
	  (if (funcall predicate x)
	      (result x)
	      (fail)))))

(defun plus (&rest parsers)
  (lambda (input)
    (loop :for parser in parsers 
          :append (funcall parser input))))

;;;; PARSER-LET* is the natural syntax for lispers
(defmacro =let* (bindings &body body)
  (if bindings
      (let ((symbol (first (first bindings))))
	`(bind ,@(cdr (first bindings))
	       (lambda (,symbol)
		 ,@(when (string-equal (symbol-name symbol) "_")
			 `((declare (ignorable ,symbol))))
	       (=let* ,(cdr bindings)
		 ,@body))))
      `(progn ,@body)))

(defun end-of-input ()
  (lambda (input)
    (when (input-empty-p input)
      (list (cons t input)))))


(defun =or (&rest parsers)
  (declare (optimize (speed 3)))
  (labels ((non-consing-or (parsers)
	   (lambda (input)
	     (or (funcall (the function (first parsers)) input) 
		 (when (rest parsers)
		   (funcall (the function (non-consing-or (rest parsers))) input))))))
    (non-consing-or parsers)))

(defun =and (p1 &rest ps)
  (=let* ((result p1))
    (if ps
	(apply #'=and ps)
	(result result))))    

(defun =not (parser)
  (lambda (input)
    (cl:when (cl:not (funcall parser input))
      (list (cons t input)))))

(defun =unless (parser &rest parsers)
  (apply #'=and (=not parser) parsers))

(defun =char (x)
  (=satisfies (lambda (y) (eql x y))))

(defun before (parser end-parser)
  (=let* ((i parser)
	  (result (lambda (input)
		    (if (funcall end-parser input)
			(list (cons i input))
			nil))))
    (result result)))

(defun =string (string)
  (if (input-empty-p string)
      (result "")
      (=let* 
	  ((_ (=char (input-first string)))
	   (_ (=string (input-rest string))))
	(result string))))

(defun =digit-char (&optional (base 10))
  (=satisfies (lambda (x) (digit-char-p x base))))

(defun digit ()
  (=let* ((char (item))
	  (digit (result (digit-char-p char))))
    (if digit (result digit) (fail))))

(defun natural-number (&optional (base 10))
  (labels ((evaluate (chars)
	     (reduce #'op (mapcar (lambda (c) (digit-char-p c base)) chars)))
	   (op (m n)
	     (+ (* base m) n)))
    (=let* ((xs (one-or-more (=digit-char base))))
      (result (evaluate xs)))))

(defun sophisticated-int ()
  (flet ((op () 
	   (plus (=let* ((_ (=char #\-)))
		   (result #'-))
		 (result #'identity))))
    (=let* ((op (op))
		  (n (natural-number)))
      (result (funcall op n)))))

(defun int ()
  (sophisticated-int))

(defun bracket (open-parser body-parser close-parser)
  (=let* ((_ open-parser)
	       (x body-parser)
	       (_ close-parser))
    (result x)))

(defun none-of (char-bag)
  (=let* ((char (item)))
    (if (not (find char char-bag))
	(result char)
	(fail))))

(defun one-of (char-bag)
  (=let* ((char (item)))
    (if (find char char-bag)
	(result char)
	(fail))))    

(defun text (&optional (parser (item)))
  (=let* ((text (one-or-more parser)))
    (result (coerce text 'cl:string))))

(defun eof (&optional (result :eof))
  (bind (end-of-input) 
	(lambda (_) (declare (ignore _ )) 
		(result result))))

(defun zero-or-more-recursive (parser &optional (combinator #'=or))
  (funcall (the function combinator)
   (=let* ((x (the function parser))
	   (y (=or (zero-or-more-recursive parser combinator)
		   (result nil))))
     (result (cons x y)))
   (result nil)))

(defun zero-or-more (parser)
  (lambda (input) :result #'identity
    (loop 
       :for value := (funcall parser input)
       :for ((result . i)) := value
       :while value :collect result :into results 
       :do (setf input i)				     
       :finally (return (list (cons results input))))))


(defun one-or-more (parser)
  (=let* ((x parser)
	  (y (zero-or-more parser)))
    (result (cons x y))))

(defun one-to (n parser)
  (case n
    (0 (result nil))
    (t (=let* ((x parser)
	       (xs (=or (one-to (1- n) parser)
			(result nil))))
	 (result (cons x xs))))))

(defun zero-to (n parser)
  (maybe (one-to n parser)))


(defun at-least (n parser &key limit)
  (case n 
    (0 (if limit 
	   (if (zerop limit)
	       (result nil)
	       (zero-to limit parser))
	   (zero-or-more parser)))
    (t (=let* ((x parser)
	       (xs (at-least (1- n) parser :limit (1- limit))))
	 (result (cons x xs))))))

(defun exactly (n parser)
  (at-least n parser :limit n))
	 
    
(defun line ()
  (=or 
   (=let* ((xs (text (none-of '(#\Newline))))
	   (end (=or (end-of-input) 
		     (=char #\Newline))))       
     (result 
      (list* :line xs (list end))))
   (bind (=char #\Newline) 
	 (constantly 
	  (result '(:line "" :terminator #\Newline))))))

(defun =progn (&rest parsers)
  (apply #'=and parsers))

(defun =prog1 (parser &rest parsers)
  (=let* ((result parser)
	  (_ (apply #'=and parsers)))
    (result result)))

(defun =prog2 (parser1 parser2 &rest parsers)
  (=and parser1 (apply #'=prog1 parser2 parsers)))

(defun string-of (parser)
  (bind parser (lambda (s) (result (coerce s 'string)))))

(defun whitespace ()
  (one-of '(#\Tab #\Newline #\Space #\Return #\Linefeed)))

(defun =list (&rest parsers)
  (if parsers 
      (=let* ((x (first parsers))
	      (xs (apply '=list (rest parsers))))
	(result (cons x xs)))
      (result nil)))

(defun call (function-designator &rest args)
  (result (apply function-designator args)))
	      

(defun skip-whitespace (parser)
  (=let* ((_ (zero-or-more (whitespace)))
	  (v parser)
	  (_ (zero-or-more (whitespace))))
    (result v)))

(defun maybe (parser)
  (=or parser (result nil)))

(defun range (from to &key (parser (item)) (predicate 'char<=))
  (=let* ((char parser))
    (if (funcall predicate from char to)
	(result char) 
	(fail))))
	
(defun org-block (&optional (level 0))
  (=or (section level)
       (simple-list)
       (text-block (line))))

(defun text-block (parser)
  (=and (=not (section-heading))
	parser))

(defun section-heading (&optional (level 0))
  (=let* ((indicator (at-least (1+ level)
			       (=char #\*)))   
	  (space (one-or-more (=char #\Space)))
	  (name (line)))
    (result (list :level (length indicator) 
		  :indicator (cons indicator space)
		  :name name))))

(defun section (&optional (level 0))
  (=let* ((heading (section-heading level))
	  (contents (zero-or-more 
		     (org-block (1+ level)))))
    (result (list :section :heading heading :contents contents))))
	
(defun section-line (&optional (level 0))
  (=and (=not (section-heading level))
	      (line)))

(defun list-item-content-line (indentation-level)
  (=let* ((indentation (at-least indentation-level (whitespace)))
	  (line (line)))
    (result (cons indentation line))))
	
(defun list-item ()
  (=let* ((pre-space (zero-or-more (whitespace)))
	  (indicator (one-of "*+-"))
	  (post-space (one-or-more (whitespace)))
	  (first-line (line))
	  (rest-lines (zero-or-more 
		       (list-item-content-line 
			(+ 1 (length pre-space)
			     (length post-space))))))
    (result (list :list-item
		  :indicator (list pre-space
				    indicator
				    post-space)
		  :content (cons (cons nil first-line) 
				 rest-lines)))))
    
(defun simple-list ()
  (=let* ((list (text-block (one-or-more (list-item)))))
    (result (cons :unordered-list list))))





    
	
		





	      







    



 
	
 
      
     


	      


			 	  
  





