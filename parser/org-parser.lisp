
(defpackage :smug.im/smug/parser/org
  (:use :cl :smug.im/smug/pure/dot))
(in-package :smug.im/smug/parser/org)

(defvar *org-parameters* (list))

(defun make-org-parameter (name value &optional documentation)
  (setf *org-parameters* (acons (cons name documentation) value *org-parameters*)))

(defun org-parameter (name)
  (cdr (assoc name *org-parameters* :key #'car)))

(defun .optional (parser)
  (.maybe parser (.result nil)))

(defun .separator ()
  (.or (.is #'char= #\Space) 
       (.progn (.not (.item))
               (.result :end))
       (.progn (.not (.is-not #'char= #\Newline))
               (.result #\Newline))))

(defstruct headline 
  stars keyword priority title tags)

(defun headline ()   
  (.let* ((STARS
           (.prog1 (.every (.is 'char= #\*))
                   (.is 'char= #\Space)))
          (KEYWORD 
           (.optional 
            (let ((keys (mapcar 
                         #'.string 
                         (org-parameter 'org-todo-keywords-1))))
              (.prog1 
               (apply #'.or keys)
               (.is #'char= #\Space))))))
    (.result (make-headline 
              :stars stars
              :keyword keyword))))
(make-org-parameter 'org-todo-keywords-1 
 '("TODO"
   "STARTED"
   "DONE")
 "The Org TODO Keywords")
