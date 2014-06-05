
(defun open-paren () 
  (=char #\())

(defun close-paren () 
  (=char #\)))

(defun non-constituent ()
  (=or (whitespace) (open-paren)  (close-paren)))

(defun constituent ()
  (=and (=not (non-constituent)) (item)))

(defun <atom> ()
  (=let* ((exp (one-or-more (constituent))))
    (result (coerce exp 'string))))

(defun sexp ()
  (skip-whitespace 
   (=or (<atom>) 
        (=let* ((_ (open-paren))
                (exp (zero-or-more (sexp)))
                (_ (close-paren)))
          (result exp)))))
