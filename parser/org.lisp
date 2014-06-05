(defpackage :smug/parser/org
  (:use :cl :smug/pure/dot)
  (:export #:.affiliated-keyword
           #:.element-block
           #:.line))
(in-package :smug/parser/org)

;; (defvar *org-parameters* (list))

;; (defun make-org-parameter (name value &optional documentation)
;;   (setf *org-parameters* (acons (cons name documentation) value *org-parameters*)))

;; (defun org-parameter (name)
;;   (cdr (assoc name *org-parameters* :key #'car)))

;; (defun .optional (parser)
;;   (.maybe parser (.result nil)))

;; (defun .separator ()
;;   (.or (.is #'char= #\Space) 
;;        (.progn (.not (.item))
;;                (.result :end))
;;        (.progn (.not (.is-not #'char= #\Newline))
;;                (.result #\Newline))))

;; (defun .make (function &rest plist)
;;   (if (and function (not plist))
;;       (.result (funcall function))
;;       (destructuring-bind (name value . rest) plist
;;         (.let* ((value value)
;;                 (rest (if rest 
;;                           (apply #'.make nil rest)
;;                           (.result nil))))
;;           (.result (if function 
;;                        (apply function name value rest)
;;                        (list* name value rest)))))))

;; (defun .make-instance (name &rest initargs)
;;  (apply #'.make (lambda (&rest initargs)
;;          (apply #'make-instance name initargs))
;;         initargs))

;; (defclass syntax () ())  
  
;; (defvar *whitespace* '(#\Space #\Tab)) 
;; 
;; (defun .whitespace ()
;;   (.is 'member *whitespace*))
;; 
;; (defclass whitespace (syntax)
;;   ((whitespace-prefix :initarg :whitespace-prefix
;;         :accessor whitespace-prefix)
;;    (whitespace-postfix :initarg :whitespace-postfix
;;          :accessor whitespace-postfix)))  

;; (defun .line (&optional (nl #\Newline))
;;   (.prog1 (.coerce (.some (.is-not #'char= nl)) 
;;                    'cl:string)
;;           (.is #'char= nl)))

;; (defstruct until 
;;   contents end)
;; 
;; (defun .until (parser &key (do (.item)) contents)
;;   (.let* ((until (.or parser (.result nil))))
;;     (if until 
;;         (.result (cons (reverse contents)
;;                        until))
;;         (.let* ((first do))
;;           (.until parser :do do :contents (list* first contents))))))  

;; 

;; (defclass greater-element (whitespace)
;;   ((keywords :initarg :keywords 
;;              :accessor greater-element-keywords)))
;; 
;; (defun .greater-element (.parser)
;;   (.let* ((keys (.some (.keyword))
;;           (pre (.some (.whitespace)))
;;           (element .parser))
;;     (setf (greater-element-keywords element) keys
;;           (whitespace-prefix element) pre)
;;     (.result element)))              

;; (defclass greater-block (greater-element)
;;   ((name :initarg :name
;;          :accessor greater-block-name)
;;    (parameters :initarg :parameters
;;          :accessor greater-block-parameters)
;;    (contents :initarg :contents
;;          :accessor greater-block-contents)))
;; 
 
;; (defvar *greater-block-classes* (list))
;; 
;; (defun greater-block-classes ()
;;   *greater-block-classes*)
;; 
;; (defun (setf greater-block-classes) (value)
;;   (setf *greater-block-classes* value))
;; 
;; (defun add-greater-block (name class)
;;   (push  (cons name class)
;;          (greater-block-classes)))
;; 
;; (defun make-greater-block-instance (name &rest initargs)
;;   (apply #'make-instance 
;;     (destructuring-bind (_ . maker)
;;         (or (assoc name (greater-block-classes) 
;;                    :test #'string-equal)
;;             (assoc nil (greater-block-classes)))
;;       (declare (ignore _))
;;       maker)            
;;     initargs))
;; 
;; 
;; 
;; 

;;      (defun .greater-block ()
;;        (flet ((.end-block (name)
;;                 (.progn (.some (.whitespace))
;;                         (.string-equal 
;;                          (concatenate 'string
;; `                                      "#+END_" name)))))
;;           
;;          (.let* ((keywords (.some (.keyword)))
;;                  (_ (.string-equal "#+BEGIN_"))
;;                  (name 
;;                   (.coerce 
;;                    (defun .name () 
;;                      (.every (.is-not 'member (cons #\Newline *whitespace*)))
;;                    'cl:string))
;;                  (parameters
;;                   (.progn (.whitespace) (.line)))
;;                  (contents (.until (.end-block name) 
;;                                    :do (.line))))
;;            (.result (make-greater-block-instance 
;;                      name
;;                      :keywords keywords
;;                      :name name
;;                      :parameters parameters
;;                      :contents (until-contents 
;;                                 contents))))))
;;         

;; (defclass special-block (greater-block) ())
;; (add-greater-block nil 'special-block)  

;; 

;; 

;; (defclass source-block (block)
;;   ((language :initarg :language 
;;              :accessor source-block-language)
;;    (switches :initarg :switches 
;;              :accessor source-block-switches)
;;    (arguments :initarg :arguments 
;;               :accessor source-block-arguments)))     
;; 
;; (add-block "SRC" 'source-block) 

;; (defmethod initialize-instance :after ((source-block source-block)
;;                                        &rest initargs)
;;   (declare (ignore initargs))
;;   (.run 
;;      (.let* ((language (.language))
;;              (_ (.optional (.is 'eql #\Space)))
;;              (switches (.switches))
;;              (_ (.optional (.is 'eql #\Space)))
;;              (arguments (.arguments)))
;;        (with-accessors ((l source-block-language)
;;                         (s source-block-switches)
;;                         (a source-block-arguments)) 
;;            source-block
;;          (.result (setf l language
;;                         s switches
;;                         a arguments))))
;;      (block-data source-block)))

;; (defparameter |*TEST-STRING:headline*|
;;   (subseq "
;; * 
;; 
;; ** DONE
;; 
;; *** Some e-mail
;; 
;; **** TODO [#A] COMMENT Title :tag:a2%: "
;;          1))
