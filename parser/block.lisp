
(defpackage :smug/parser/org/block 
  (:use :cl 
        :smug/parser/org/dot
        :smug/parser/org/util
        :smug/parser/org/parameters)
  (:shadow #:block))
(in-package :smug/parser/org/block)

(defpackage :smug/parser/org/block 
  (:use :cl 
        :smug/parser/org/dot
        :smug/parser/org/util
        :smug/parser/org/parameters)
  (:shadow #:block))
(in-package :smug/parser/org/block)

(defclass block (whitespace) 
 ((name :accessor block-name)
  (data :initarg :parameters
              :accessor block-data)
  (contents :initarg :contents
            :accessor element-block-contents)))

(defclass element-block (greater-block) 
 ((name :accessor element-block-name)
  (parameters :initarg :parameters
              :accessor element-block-data)
  (contents :initarg :contents
            :accessor element-block-contents)))
