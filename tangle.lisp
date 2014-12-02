(defpackage :smug/tangle
  (:use :cl :smug/tutorial)
  (:export))
(in-package :smug/tangle) 

(defun .line ()
  (.prog1 (.map 'string (.is-not 'char= #\newline) :at-least 0)
          (.char= #\newline)))

(defparameter *whitespace* '(#\space #\tab))

(defun .whitespace (&key (result-type 'string) (at-least 1))
  (.map result-type (.is 'member *whitespace*) :at-least at-least))

(defun .atom ()
  (.first (.map 'string (.is-not 'member (cons #\Newline *whitespace*))
                :at-least 1)))


(defun whitespace-prefix-number (lines)
  (let ((nums (loop :for line 
                 :in (remove "" lines  :test #'string=)
                 :collect (caar (run (.let* ((ws (.whitespace :at-least 0)))
                                        (.result (length ws))) line)))))
   (if nums 
       (apply #'min nums) 
       0)))
  
(defun whitespace-prefix-trim (lines)
  (let ((num (whitespace-prefix-number lines)))
    (mapcar (lambda (l) (if (string= "" l)
                            l
                            (subseq l num)))
                    lines)))

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


(defun |#+NAME: | ()
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

(defun .code-block-plist ()
   (.let* ((name (.optional (|#+NAME: |)))
           (begin (|#+BEGIN_SRC <language> <switches> <header arguments>|))
           (body  (|<body> #+END_SRC|)))
     (.result (list* :name name :body body begin))))

(defstruct code-block 
  name
  language
  switches
  header-arguments
  body)

(defun .code-block ()
  (.let* ((plist (.code-block-plist)))
    (.result (apply 'make-code-block plist))))

(defun getf-code-block (cb indicator)
  (let* ((db (code-block-header-arguments cb))
         (value (when (listp db) (assoc indicator db :test 'string-equal))))
    (values (cdr value) (car value))))


;; #+quicklisp (ql:quickload "alexandria")

(defun org-code-blocks (org-doc)
  (let ((string 
         (etypecase org-doc
           (string org-doc)
           (pathname (alexandria:read-file-into-string org-doc)))))
    (destructuring-bind ((list . input))
        (run (.first (.map 'list (.or (.code-block) (.line)))) string)
      (values (remove-if-not #'code-block-p list) input))))
 


(defparameter *code-blocks* 
  (org-code-blocks (merge-pathnames 
                    "doc/tutorial.org" 
                    (asdf:system-source-directory :smug))))  

(defun code-blocks-with-tangle (&optional (code-blocks *code-blocks*))
  (remove-if-not (lambda (cb) (getf-code-block cb "tangle"))
                 code-blocks))

(defun code-block-noweb-body (code-block)
  (mapcan (lambda (line) (or (caar (run (.noweb) line)) (list line)))
          (whitespace-prefix-trim (code-block-body code-block))))

(defun .noweb (&optional (code-blocks  *code-blocks*))
  (.let* ((prefix (.map 'string (.item) :at-least 0))
          (name 
           (.prog2 (.string= "<<")
                   (.map 'string (.item))
                   (.string= ">>")))
            (body 
             (let ((blocks
                       (remove-if-not (lambda (cb) (string= name (code-block-name cb)))
                                code-blocks)))
               (.result (and blocks (loop for block in blocks :append 
                                     (whitespace-prefix-trim
                                    (code-block-noweb-body block)))))))
          (new-body (.result (loop :for line :in body 
                                :collect (concatenate 'string prefix line))))
          (postfix (.first (.map 'string (.item) :at-least 0))))
    (.result (if new-body 
                 (prog1 new-body
                   (setf (car (last new-body))
                         (concatenate 
                          'string 
                          (car (last new-body))
                          postfix)))
                 (list (concatenate 'string prefix postfix))))))
