
(defpackage :smug/parser/org/section
  (:use :cl
        :smug/parser/org/dot 
        :smug/parser/org/util
        :smug/parser/org/syntax)
  (:import-from :smug/parser/org/headline 
                #:.headline)
  (:import-from :smug/parser/org/block 
                          #:.block
                          #:block-name)
  (:export #:section
           #:.section))
(in-package :smug/parser/org/section)

(defclass section (syntax) 
  ((contents :accessor section-contents 
             :initarg :contents)))

(defun .section-content ()
  (.progn (.not (.headline))
          (.or (.block) (.line))))

(defun find-block (section &key type name number)
  (etypecase section 
    (list (let* ((blocks (remove-if-not (lambda (thing)
                                          (typep thing 'smug/parser/org/block:block))
                                        section))
                 (numbered (when number (nth (1- number) blocks))))
            numbered)
          )))
            
       
(defun .section 
    (&key 
       (content (.section-content)))
  (.every content))
