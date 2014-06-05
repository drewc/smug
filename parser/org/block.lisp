(defpackage :smug/parser/org/block
  (:use :cl 
        :smug/parser/org/dot
        :smug/parser/org/util
        :smug/parser/org/parameters
        :smug/parser/org/syntax
        :smug/parser/org/environment)
  (:import-from :smug/parser/org/keyword
                #:.keyword
                #:keywords)
  (:shadow #:block)
  (:export #:block
           #:block-contents
           #:.block
           #:add-block
           #:comment-block
           #:verse-block
           #:example-block))
(in-package :smug/parser/org/block)

(defclass block (environment keywords)
  ((begin :initarg :begin)
   (name :initarg :name 
         :accessor block-name)
   (data :initarg :data
         :accessor block-data)
   (contents-start-input
    :initarg :contents-start-input
    :accessor block-contents-start-input)
   (contents :initarg :contents
             :accessor block-contents)
   (contents-end-input
         :initarg :contents-end-input
         :accessor block-contents-end-input)
   (end :initarg :end)
   (start-input :initarg :start-input 
                :accessor block-start-input)
   (end-input :initarg :end-input
              :accessor block-end-input)))

(defun .name () 
  (.string-of (.is-not 'member (cons #\Newline *whitespace*))))

(defun .end-block (name)
  (.let* ((prefix (.string-of (.whitespace)))
          (end (.string-equal 
                (concatenate 
                 'string  "#+END_" name))))
    (.result (concatenate 'string prefix end))))
                 
(defun .block ()
  (.let* ((start-input (.input))
          (pre (.some (.whitespace)))
          (begin (.string-equal "#+BEGIN_"))
          (keywords (.some (.keyword)))
          (name (.name))
          (data (.prog2 (.is 'char= #\Space)
                        (.string-of (.is-not 'char= #\Newline))
                        (.is 'char= #\Newline)))
          (contents-start-input (.input))
          (contents.input.end (.until 
                               (.let* ((eip (.input))
                                       (end
                                        (.end-block name)))
                                 (.result (cons eip end)))
                              :do (.line)))
          (post (.some (.is-not 'char= #\Newline)))
          (end-input (.input)))
    (.result 
     (make-instance 
     (or (block-class name) 'block)
     :start-input start-input
     :whitespace-prefix pre
     :begin begin
     :keywords keywords
     :name name
     :data data
     :contents-start-input contents-start-input
     :contents (car contents.input.end)
     :contents-end-input (cadr contents.input.end)
     :end (cddr contents.input.end)
     :whitespace-postfix post
     :end-input end-input))))
   
   
(defvar *block-classes* (list))

(defun add-block (name class)
  (pushnew (cons name class)
           *block-classes* 
           :test #'equalp))

(defun block-class (name)
  (cdr (assoc name *block-classes* 
              :test #'string-equal)))
   

(defclass comment-block (block) ())
(add-block "COMMENT" 'comment-block)

(defclass example-block (block) ())
(add-block "EXAMPLE" 'example-block)

(defclass verse-block (block) ())
(add-block "VERSE" 'verse-block)
