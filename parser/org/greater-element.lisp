(defpackage smug/parser/org/greater-element 
  (:use :cl :smug/parser/org/dot
        :smug/parser/org/util
        :smug/parser/org/parameters))

(defclass greater-element (whitespace)
  ((keywords :initarg :keywords 
             :accessor greater-element-keywords)))

(defun .greater-element (.parser)
  (.let* ((keys (.some (.keyword))
          (pre (.some (.whitespace)))
          (element .parser))
    (setf (greater-element-keywords element) keys
          (whitespace-prefix element) pre)
    (.result element)))
