
(defpackage :smug/pure/dot
    (:use)
    (:import-from :smug/pure)
    #.`(:export 
	#:.let*
	#:.or 
	#:.result
	#:.zero
	#:.plus
	#:.item
	#:.input
	#:.<parser>
	,@(when (find-package :smug/pure/dot)
                        (let (export)
                          (do-external-symbols (s :smug/pure/dot)
                            (push s export))
                          (nreverse export)))))
   
(in-package :smug/pure)
    
(eval-when (:compile-toplevel :load-toplevel :execute)
  (do-external-symbols (symbol :smug/pure)
    (let ((dot-symbol (intern (format nil ".~S" symbol) :smug/pure/dot)))
      (when (fboundp symbol)
	(if (macro-function symbol)
	    (setf (macro-function dot-symbol) (macro-function symbol))
	    (setf (fdefinition dot-symbol) (fdefinition symbol))))
      (eval `(define-symbol-macro ,dot-symbol ,symbol))
      (export dot-symbol :smug/pure/dot))))
  
(defun smug/pure/dot:.result (value)
    (smug/pure/monad:result 
     smug/pure/dot:.<parser> value))

(defun smug/pure/dot:.item ()
    (smug/pure:item
     smug/pure/dot:.<parser>))

(defun smug/pure/dot:.input ()
  (smug/pure:input
   smug/pure/dot:.<parser>))
  
(defun smug/pure/dot:.zero ()
  (smug/pure/monad:zero 
   smug/pure/dot:.<parser>))
  
(defun smug/pure/dot:.plus (parser1 parser2)
  (smug/pure/monad:plus
     smug/pure/dot:.<parser> parser1 parser2))
  
(defun smug/pure/dot:.or (&rest parsers)
  (if (cl:rest parsers)      
      (maybe (cl:first parsers)
	     (apply 'smug/pure/dot:.or 
		    (cl:rest parsers)))
      (cl:first parsers)))
  
(defun %.symbol-name (s)
  (concatenate 
   'cl:string "." (symbol-name s)))
  
(defmacro smug/pure/dot::.let* (bindings &body body)
  `(mlet* (SMUG/PURE:<PARSER> :package :smug/pure/dot
			      :symbol-name %.symbol-name)
       ,bindings ,@body))
  
  
  
  (export 'smug/pure/dot::.let* :smug/pure/dot)
