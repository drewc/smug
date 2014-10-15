(defpackage :smug/weave
  (:use :cl :smug/tutorial)
  (:import-from :alexandria)
  (:import-from :yasexml
                #:<>)
  (:export))
(in-package :smug/weave) 

(defun .document-line ()
  (.or (.let* ((text (.map 'list (.is-not #'char= #\Newline)))
               (newline (.or (.char= #\Newline)
                             (.and (.not (.item)) 
                                   (.result '())))))
         (.result (concatenate 'string text (when newline (string newline)))))
       (.let* ((char (.char= #\Newline)))
         (.result (string char)))))

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
                         (.document-line)
                         (.fail))))
     (.result (make-headline :stars stars
                             :text (string-right-trim '(#\Newline) headline)))))

(defun .outline-line ()
  (.and (.not (.stars))
        (.document-line)))

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
