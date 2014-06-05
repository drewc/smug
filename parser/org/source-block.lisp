(defpackage :smug/parser/org/source-block
  (:use :cl 
        :smug/parser/org/dot
        :smug/parser/org/util
        :smug/parser/org/parameters
        :smug/parser/org/syntax
        :smug/parser/org/environment)
  (:import-from :smug/parser/org/keyword
                #:.keyword
                #:keywords)
  (:shadowing-import-from :smug/parser/org/block 
                          #:block #:add-block #:block-data)
  (:export #:source-block
           #:source-block-language))
(in-package :smug/parser/org/source-block)

(defclass source-block (block)
  ((language :initarg :language 
             :accessor source-block-language)
   (switches :initarg :switches 
             :accessor source-block-switches)
   (arguments :initarg :arguments 
              :accessor source-block-arguments)))     

(add-block "SRC" 'source-block) 

(defun .language () 
  (.string-of (.is-not 'member (cons #\Newline *whitespace*))))

(defun .switches () 
  ;; TODO
  (.result nil))

(defun .arguments () 
 (.optional (.string-of (.is-not 'eql #\Newline))))

(defmethod initialize-instance :after ((source-block source-block)
                                       &rest initargs)
  (declare (ignore initargs))
  (.run 
     (.let* ((language (.language))
             (_ (.optional (.is 'eql #\Space)))
             (switches (.switches))
             (_ (.optional (.is 'eql #\Space)))
             (arguments (.arguments)))
       (with-accessors ((l source-block-language)
                        (s source-block-switches)
                        (a source-block-arguments)) 
           source-block
         (.result (setf l language
                        s switches
                        a arguments))))
     (block-data source-block)))
