
;; [[id:5762b879-10fc-47ac-b8a9-59007996d49b][greater-element]]

(defstruct (greater-element (:include whitespace)))

(defun .greater-element (.parser)
  (.let* ((pre (.whitespace))
          (element .parser)
          (post (.progn
                 (.guard 'greater-element-p element)
                 (.whitespace))))
    (setf (whitespace-pre element) pre
          (whitespace-post element) post)
    (.result element)))

;; greater-element ends here
