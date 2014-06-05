(defpackage :smug/parser/org/keyword
  (:use :cl 
        :smug/parser/org/dot
        :smug/parser/org/util
        :smug/parser/org/parameters
        :smug/parser/org/syntax)
  (:shadow #:keyword)
  (:export #:keyword 
           #:keywords
           ))
(in-package :smug/parser/org/keyword)


(defclass keywords ()
  ((keywords :initarg :keywords
             :accessor keywords)))

(defun get-keyword (key keywords)
  (let ((list (etypecase keywords
                (list keywords)
                (keywords (keywords keywords)))))

    (find key list
          :test 'string-equal 
          :key 'keyword-key)))

(defun name (environment)
  (get-keyword "name" environment))      

(defclass keyword (whitespace)
  ((key :initarg :key
         :accessor keyword-key)
   (value :initarg :value
          :accessor keyword-value)))
     
(defun .keyword ()
  (.make-instance 
   'keyword
   :whitespace-prefix (.some (.whitespace))
   :key (.progn          
           (.string= "#+")
           (.coerce (.every (.is-not 'char= #\:))
                    'cl:string))
   :value (.progn 
           (.string= ": ")
           (.line))))
