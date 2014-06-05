
(defpackage :smug/parser/org/environment
  (:use :cl  
        :smug/parser/org/util
        :smug/parser/org/syntax)
  (:export #:environment))
(in-package :smug/parser/org/environment)

(defclass environment (whitespace syntax) ())
