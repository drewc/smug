(defpackage :smug/parse/org
  (:use :cl :smug/tutorial)
  (:import-from :smug/parse/outline
                #:.outline 
                #:outline
                #:outline-headline
                #:outline-text
                #:outline-sub-outlines
                #:headline
                #:headline-stars
                #:headline-text)
  (:import-from :alexandria)
  (:import-from :yasexml
                #:<>)
  (:export #:org-section-headline
           #:org-section-sections
           #:org-file))
(in-package :smug/parse/org)
 
(defun loop-and-concatenate (list &aux body text)
  (if (stringp list) 
       list
       (flet ((text () 
                (when text                        
                  (push (apply #'concatenate 'string 
                               (mapcar #'string (nreverse text)))
                        body)
                  (setf text nil))))
         (let ((result 
                (loop :for thing in list
                   :do  (typecase thing
                          ((or character string)
                           (push thing text))
                          (list 
                           (let ((result (loop-and-concatenate thing)))
                             (typecase result
                               (string 
                                (push result text))
                               (t (text)
                                  (push result body)))))
                             (t (text) (push thing body)))
                   :finally 
                   (text) (return (nreverse body)))))
           (if (rest result) result (first result))))))


(defun .line ()
  (.let* ((text (.optional 
                 (.first (.map 'list (.is-not #'char= #\Newline)))))
          (newline (.or (.char= #\Newline)
                        (.and (.not (.item)) 
                              (.result '())))))
         (if (or text newline)
             (.result (concatenate 'string text (when newline (string newline))))
             (.fail))))

(defstruct setting
  name words)

(defun .setting ()
  (.let* ((name 
           (.prog2 (.string= "#+")
                   (.map 'string (.is-not 'member '(#\: #\Space #\Newline)))
                   (.string= ": ")))
          (words (.line)))
    (.result (make-setting 
              :name name 
              :words (string-right-trim '(#\Newline) words)))))

(defvar *org-file-settings*)

(defun .org-file-setting ()
  (.let* ((setting (.setting))
          (existing (.result (when (boundp '*org-file-settings*)
                               (find (setting-name setting) *org-file-settings*
                                     :key 'setting-name 
                                     :test #'string-equal))))
          (_ (.result (if existing
                          (setf (setting-words existing)
                                (concatenate 'string (setting-words existing)
                                             " " (setting-words setting)))
                          (when (boundp '*org-file-settings*)
                            (push setting *org-file-settings*))))))
    (unless (boundp '*org-file-settings*)
      (error "org-file-settings is not bound"))
    
    (.result (copy-setting setting))))

(defparameter *org-object-parsers* 
  '(.footnote-reference
    .org-file-setting
    .markup .quote))  
(defun .org-object ()
  
  (let ((list (mapcar #'funcall *org-object-parsers*)))
    (setf (cdr (last list)) (list (.is 'char= #\Newline)))      
    (apply #'.or list)))

(defun .org-line ()
  (.or (.let* ((line (.first 
                      (.map 'list (.or (.org-inline)
                                       (.is-not 'char= #\Newline))))))
                    (.result line))
       (.line)))


(defstruct markup 
  text style char)

(defun .markup () 
  (flet ((.m (style char)
           (.let* ((text (.progn (.is 'char= char)
                                 (.map 'string (.is-not 'member (list char #\Newline)))))
                   (close (.prog1
                           (.is-not 'char= #\space)
                           (.is 'char= char))))
             (.result (make-markup 
                       :style style
                       :char char
                       :text (concatenate 'string text (string close)))))))
    (.or (.m :bold #\*)
         (.m :italic #\/)
         (.m :underlined #\_)
         (.m :verbatim #\=)
         (.m :code #\~)
         (.m :strike-through #\+))))

(defstruct footnote-reference 
  marker)

(defun .footnote-marker ()
  (.or (.map 'string (.is 'digit-char-p))
       (.concatenate 'string (.string= "fn:")
                     (.map 'string (.is-not 'member '(#\Newline #\]))))))

(defun .footnote-reference () 
  (.let* ((marker (.prog2 (.char= #\[)
                      (.footnote-marker)
                    (.char= #\]))))
    (.result (make-footnote-reference :marker marker))))
    

(defun .org-inline ()
  (.or (.markup)
       (.footnote-reference)))



(defstruct org-headline 
 stars body)

(defstruct org-section 
  headline
  body sections)

(defstruct org-file
 body sections
  settings)    


(defstruct org-headline 
 stars body)

(defstruct org-section 
  headline
  body sections)

(defstruct org-file
 body sections
  settings)    


(defun .org-body ()
  (.let* ((body (.first (.map 'list (.org-element)))))
    (.result (loop-and-concatenate body))))
(defun .org-element ()
  (.or 
   (.org-file-setting)
   (.quote)
   (.code-block)
   (.org-line)))

(defun org-section (outline)
  (let* ((headline (outline-headline outline))
         (headline-text (headline-text headline))
         (headline-body (when headline-text 
                          (parse (.org-body) headline-text)))
         (text (outline-text outline))
         (body (when text (parse (.org-body) text)))
         (sections (mapcar #'org-section (outline-sub-outlines outline))))
    (make-org-section :headline (make-org-headline
                                 :stars (headline-stars headline)
                                 :body headline-body)
                      :body (loop-and-concatenate body)
                      :sections sections)))

(defun org-file (outline)
  (let* (*org-file-settings*
         (text (outline-text outline))
         (body (when text (parse (.org-body) text)))
         (sections (mapcar #'org-section (outline-sub-outlines outline))))
    (make-org-file 
     :body (loop-and-concatenate body)
     :sections sections
     :settings *org-file-settings*)))

(defstruct org-document
   body 
   sections
   settings)
(defgeneric org-document (thing)
  (:method ((outline outline))
    (let* (*org-file-settings*
           (text (outline-text outline))
           (body (parse (.org-body) text))
           (sections (mapcar #'org-section (outline-sub-outlines outline))))
      (make-org-document 
       :body (loop-and-concatenate body)
       :sections sections
       :settings *org-file-settings*)))
  (:method ((string string))
    (org-document (parse (.outline) string)))
  (:method ((stream stream))
    (org-document 
     (with-output-to-string (string)
       (alexandria:copy-stream stream string))))
  (:method ((pathname pathname))
    (org-document (alexandria:read-file-into-string pathname))))

(defstruct code-block 
  name
  language
  switches
  header-arguments
  body)

(defun .code-block ()
    (.let* ((name (.optional 
                   (|#+NAME: <name>|)))
            (begin (|#+BEGIN_SRC <language> <switches> <header arguments>|))
            (body  (|<body> #+END_SRC|)))
       (.result (make-code-block :name name :body body begin))))


(defun |#+NAME: <name>| ()
  (.progn (.optional (.whitespace))
          (.string-equal "#+NAME: ")
          (.prog1 (.map 'string (.is-not 'char= #\Newline))
            (.char= #\Newline))))
 
(defun |#+BEGIN_SRC <language> <switches> <header arguments>| ()
    (.let* ((language (|#+BEGIN_SRC <language>|))
            (switches (|<switches>|))
            (args (|<header arguments>|)))
      (.result (list :language language
                     :switches switches
                     :header-arguments args))))
(defun |#+BEGIN_SRC <language>| ()
  (.progn (.optional (.whitespace))
          (.string-equal "#+BEGIN_SRC ")
          (.atom)))

(defun |<body> #+END_SRC| ()
  (flet ((.end () 
           (.prog2 (.optional (.whitespace))
               (.string-equal "#+END_SRC") 
              (.or (.is 'member '(#\space #\newline))
                   (.not (.item))))))
    (.prog1 
     (.map 'list (.and (.not (.end))
                       (.line)))
     (.end))))

(defun |<switches>| ()
  (flet ((.switch ()
           (.or
            (.string= " -n")
            (.string= " +n")
            (.string= " -r") ;; (ref:switch)
            (.let* ((-l (.string= " -l "))
                    (format (.prog2 (.char= #\")
                                    (.map 'string (.is-not 'char= #\"))
                                    (.char= #\"))))
              (.result (list :switch -l
                             :format format))))))
    (.map 'list 
          (.let* ((switch (.switch)))
            (.result (if (stringp switch) 
                         (list :switch switch)
                         switch)))
          :at-least 0)))

(defun |<header arguments>| ()
  (flet ((.arg ()
           (.let* ((key (.progn 
                         (.optional (.whitespace))
                         (.char= #\:)
                         (.atom)))
                   (value (.progn 
                           (.whitespace) 
                           (.map 'string (.is-not 'member '(#\Newline #\:))))))
             (.result (cons key value)))))
    (.or (.prog1 (.first (.map 'list (.arg)))
            (.char= #\Newline))
         (.line))))

(defparameter *whitespace* '(#\space #\tab))

(defun .whitespace (&key (result-type 'string) (at-least 1))
  (.map result-type (.is 'member *whitespace*) :at-least at-least))

(defun .atom ()
  (.first (.map 'string (.is-not 'member (cons #\Newline *whitespace*))
                :at-least 1)))
