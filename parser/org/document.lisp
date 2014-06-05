
(defpackage :smug/parser/org/document 
  (:use :cl :smug/parser/org/dot 
        :smug/parser/org/util
        :smug/parser/org/syntax)
  (:import-from :smug/parser/org/section
                #:.section)
  (:import-from :smug/file)
  (:import-from :smug/parser/org/outline
                #:.outline
                #:outline-headline
                #:outline-sub-outlines)
  (:import-from :smug/parser/org/headline 
                #:headline-title
                #:headline-stars
                #:headline-level)
  (:import-from :smug/parser/org/keyword
                #:.keyword #:keywords
                #:get-keyword
                #:keyword-value)
  (:export #:document
           #:read-document
           #:.document
           #:document-section
           #:document-outlines
           #:document-title
           #:find-outline))
(in-package :smug/parser/org/document)

(defclass document (keywords syntax)
  ((section :accessor document-section
            :initarg :section)
   (outlines :accessor document-outlines
             :initarg :outlines)
   (pathname :accessor document-pathname 
             :initarg :pathname
             :initform nil)))

(defun document-title (document)
  (or 
   (ignore-errors (keyword-value (get-keyword "TITLE" document)))
   (first (document-section document))
   (document-pathname document)))

(defun .document (&key (section (.section)))
  (.make-instance 
   'document 
   :keywords (.some (.keyword))
   :section (.section)
   :outlines (.some (smug/parser/org/outline:.outline))))

(defun find-outline (headline document)
  (block outline 
    (labels 
        ((findo (outlines)
           (find headline outlines
                 :test 
                 (lambda (headline outline) 
                   (or (when (string= headline 
                                      (headline-title (outline-headline outline)))
                         (return-from outline outline))
                       (findo (outline-sub-outlines outline)))))))
      (findo (document-outlines document)))))
                                  

(defun read-document (source &key (pathname))
  (let* ((source-string (etypecase source
                          (pathname (smug/file:read-file-into-string source))
                          (t source))))
    (multiple-value-bind (document leftover others)
        (.run (.document) source-string)
      (multiple-value-prog1 (values document leftover others)
        (when (or pathname (pathnamep source))
          (setf (document-pathname document) source))))))
