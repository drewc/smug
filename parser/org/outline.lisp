(defpackage :smug/parser/org/outline 
  (:use :cl
        :smug/parser/org/dot 
        :smug/parser/org/util
        :smug/parser/org/syntax)
  (:import-from :smug/parser/org/headline 
                #:.headline
                #:headline-stars
                #:headline-level)
  (:import-from :smug/parser/org/section
                #:.section)
  (:export #:outline
           #:outline-headline
           #:outline-section
           #:outline-start-input
           #:outline-end-input
           #:outline-sub-outlines
           #:.outline))
(in-package :smug/parser/org/outline)

(defclass outline (syntax) 
  ((headline :accessor outline-headline
             :initarg :headline)
   (section :accessor outline-section
            :initarg :section)
   (sub-outlines :accessor outline-sub-outlines)
   (start-input :accessor outline-start-input 
          :initarg :start-input)
   (end-input :accessor outline-end-input)))

  
(defun .same-level-or-less-headline (headline)
  (.let* ((new-headline (.headline)))
    (.guard #'identity 
            (<= (headline-level new-headline)
                (headline-level headline)))))
    
(defun .outline (&key 
                   (headline (.headline))
                   (section (.optional (.section))))
  
  (.let* ((outline 
           (.make-instance 'outline
                           :start-input (.input)
                           :headline headline
                           :section section))
          (subs (.some
                 (.progn (.not (.same-level-or-less-headline
                                (outline-headline outline)))
                         (.outline))))
          (end (.input)))
    (prog1 (.result outline)
      (setf (outline-sub-outlines outline) subs
            (outline-end-input outline) end))))
