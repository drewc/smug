(defpackage :smug/parser/org/affiliated-keyword
  (:use :cl 
        :smug/parser/org/dot
        :smug/parser/org/util
        :smug/parser/org/parameters)
  (:export #:))
(in-package :smug/parser/org/affiliated-keyword)

(defclass |keyword| (whitespace)
  ((name :initarg :name
         :accessor keyword-name)
   (value :initarg :value
          :accessor keyword-value)))
     
(defun .keyword ()
  (.make-instance 
   '|keyword|
   :whitespace-prefix (.some (.whitespace))
   :name (.coerce 
          (.progn          
           (.string= "#+")
           (.every (.is-not 'member '(#\Newline #\:))))
          'cl:string)
   :value (.progn 
           (.string= ": ")
           (.line))))

(defun |keywordp| (thing)
  (typep thing '|keyword|))
