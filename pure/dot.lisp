
(defpackage :drewc.org/smug/pure/dot
  (:use)
  (:import-from :drewc.org/smug/pure)
  #.`(:export ,@(when (find-package :drewc.org/smug/pure/dot)
                      (let (export)
                        (do-external-symbols (s :drewc.org/smug/pure/dot)
                          (push s export))
                        (nreverse export)))))
 
(in-package :drewc.org/smug/pure)
  
(do-external-symbols (symbol :drewc.org/smug/pure)
  (let ((dot-symbol (intern (format nil ".~S" symbol) :drewc.org/smug/pure/dot)))
    (when (fboundp symbol)
      (if (macro-function symbol)
          (setf (macro-function dot-symbol) (macro-function symbol))
          (setf (fdefinition dot-symbol) (fdefinition symbol))))
    (eval `(define-symbol-macro ,dot-symbol ,symbol))
    (export dot-symbol :drewc.org/smug/pure/dot))
  )

(defmacro drewc.org/smug/pure/dot::.let* (bindings &body body)
  `(mlet* (<parser> :package :drewc.org/smug/pure/dot
                    :symbol-name #.(lambda (s)
                                     (concatenate 
                                      'string "." (symbol-name s))))
       ,bindings ,@body))

(export 'drewc.org/smug/pure/dot::.let* :drewc.org/smug/pure/dot)
