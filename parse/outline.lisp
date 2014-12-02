(defpackage :smug/parse/outline
  (:use :cl :smug/tutorial)
  (:export))
(in-package :smug/parse/outline) 

(defun .line ()
  (.let* ((text (.optional 
                 (.first (.map 'list (.is-not #'char= #\Newline)))))
          (newline (.or (.char= #\Newline)
                        (.and (.not (.item)) 
                              (.result '())))))
         (if (or text newline)
             (.result (concatenate 'string text (when newline (string newline))))
             (.fail))))

(defun .stars ()
  (.prog1 (.map 'string (.char= #\*))
          (.char= #\Space)))                           

(defstruct headline
  (stars (list))
  text)
 
(defun headline-level (headline)
  (length (headline-stars headline)))

(defun .sub-headline (&key (from-level 0))
   (.let* ((stars (.stars))
           (headline (if (< from-level (length stars))
                         (.line)
                         (.fail))))
     (.result (make-headline :stars stars
                             :text (string-right-trim '(#\Newline) headline)))))

(defun .outline-line ()
  (.and (.not (.stars))
        (.line)))

(defun .outline-text ()
  (.let* ((lines (.first (.map 'list (.outline-line)))))
    (.result (apply #'concatenate 'string lines)))) 

(defstruct outline 
  (headline nil)
  text 
  sub-outlines)

(defun .outline (&key from-level)
    (.let* ((headline 
             (if from-level
                 (.sub-headline :from-level from-level)
                 (.result nil)))
            (text (.optional (.outline-text)))
            (sub-outlines 
             (let ((new-level (if headline 
                                  (headline-level headline)
                                  0)))
               (.optional (.first (.map 'list (.outline :from-level new-level)))))))
      (.result (make-outline :headline headline 
                             :text text 
                             :sub-outlines sub-outlines))))
