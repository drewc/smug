(defpackage :smug/parser/org/headline
  (:use :cl 
        :smug/parser/org/dot
        :smug/parser/org/util
        :smug/parser/org/parameters)
  (:export #:.headline
           #:headline
           #:headline-stars
           #:headline-title))
(in-package :smug/parser/org/headline)

(defstruct headline 
  stars keyword priority title tags)

(defun headline-level (headline)
  (length (headline-stars headline)))

(defun .headline ()
  (.prog1 
   (.make 
    'make-headline
    :stars (.stars)
    :keyword (.optional (.keyword))
    :priority (.optional (.priority))
    :title (.optional (.title))
    :tags (.optional (.tags)))
   (.some (.is 'char= #\Space))
     (.or  (.not (.item))
           (.is 'char= #\Newline))))

(defun .stars ()
  (.prog1 
   (.every (.is 'char= #\*))
   (.is 'char= #\Space)))

(defun .keyword () 
  (.prog1 
   (apply #'.or (mapcar #'.string= (org-parameter 'org-todo-keywords-1)))
   (.is #'char= #\Space)))
(defun .priority ()
  (.prog2
   (.string= "[#")
   (.is 'alpha-char-p)
   (.string= "] ")))

(defun .title ()
  (.coerce (.every 
            (.progn (.not (.tags)) 
                    (.is-not 'char= #\Newline)))
           'cl:string))
(defun .tag () 
  (.progn (.is 'char= #\:)
          (.coerce 
           (.every 
            (.or (.is 'alphanumericp)
                 (.is 'member '(#\_ #\@ #\# #\%))))
           'cl:string)))

(defun .tags ()
  (.prog1 (.every (.tag))
          (.is 'char= #\:)))                 


(define-org-parameter 
  org-todo-keywords-1 
 '("TODO"
   "STARTED"
   "DONE")
 "The Org TODO Keywords")
