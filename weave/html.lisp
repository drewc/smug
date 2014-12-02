(defpackage :smug/weave/html
  (:use :cl :smug/tutorial :smug/parse/org)

  (:import-from :smug/parse/outline
                #:.outline)
  (:import-from :smug/parse/org
                #:org-file
                #:setting
                #:setting-name
                #:setting-words)
  (:import-from :alexandria)
  (:import-from :yasexml
                #:<>)
  (:import-from :closure-html)
  (:export))
(in-package :smug/weave/html) 

(defgeneric html (object &key type)
  (:method (object &key &allow-other-keys)
    (<> (:text (princ-to-string object))))
  (:method ((object list)
            &key &allow-other-keys)
    (dolist (item object)
      (html item))))


(import '(smug/parse/org::markup
          smug/parse/org::markup-text))

(defmethod html ((object markup) &key &allow-other-keys)
  (<> (:text (markup-text object))))
    

(defun whitespace-as-underscore (string &optional (whitespace '(#\space #\tab)))
  (labels ((.wau ()
             (.let* ((chars (.first (.map 'string (.is-not 'member whitespace))))
                     (whitespace (.optional (.first (.map 'string (.is 'member whitespace)))))
                     (rest (.optional (.wau))))
               (.result (concatenate 
                         'string chars 
                         (make-list (length whitespace)
                                    :initial-element #\_)
                         rest)))))
    (remove-if (lambda (i) (find i "~:+#.!|<>\"()'*,?/`"))
               (or (parse (.wau) string)
                   string))))

(defun make-id-hashes ()
  (cons (make-hash-table :test #'equal)
        (make-hash-table :test #'equalp)))

(defparameter *unique-ids* (make-id-hashes))

(defun unique-id (outline  
                  &key (id-hashes *unique-ids*)
                    (headline (smug/parse/org::org-section-headline outline))
                    (body (smug/parse/org::org-headline-body headline))
                    (headline-text 
                     (let ((body (if (listp body)
                                     body
                                     (list body))))
                       (apply #'concatenate 'string
                              (mapcar (lambda (i) 
                                        (typecase i 
                                          (smug/parse/org::markup
                                           (smug/parse/org::markup-text i))
                                          (t i))) 
                                      body
                                    )))))
  (destructuring-bind (text-hash . outline-hash)
      id-hashes
    (let* ((unique? (gethash outline outline-hash))
           (id? (unless unique? (gethash headline-text text-hash)))
           (id (unless unique? 
                (if id? 
                    (unique-id outline :id-hashes id-hashes 
                               :headline-text (concatenate 
                                               'string headline-text
                                               (smug/parse/org::org-headline-stars headline)))
                    (whitespace-as-underscore headline-text)))))
      (or unique?
          (progn 
            (push id (gethash headline-text text-hash))
            (setf (gethash outline outline-hash) id))))))
      
  

  
(import '(smug/parse/org::org-document-body
          smug/parse/org::org-file
          smug/parse/org::org-document-settings
          smug/parse/org::setting-name
          smug/parse/org::setting-words))

(defun <org-document-body/> (org)
  (let* ((body (org-document-body org))
         (title (find :title (org-document-settings org) 
                      :key 'setting-name 
                      :test #'string-equal)))
         ;; (headline (org-headline org))
         ;; (stars (when headline (headline-stars headline)))
         ;; (level (length stars))
         ;; (class (format nil "~R-star~:P" level)))    
    (<> (div :class "class")
      #+(or)(when headline
       (<> `(,(concatenate 
              'string "h" (princ-to-string
                           (if (zerop level)
                               1
                               (apply #'min (list level 6)))))
             :class ,class)
        (<> (:text (headline-text headline)))))
      (<> h1 (<> (:text (setting-words title))))
      (<org-body/> body))))

(defun <org-body/> (body)
  (<> (div :class "org-body")
    (<org-<p>s> body)))


(import '(smug/parse/org::org-section
          smug/parse/org::org-section-headline
          smug/parse/org::org-section-body
          smug/parse/org::org-section-sections
          smug/parse/org::org-document-sections
          smug/parse/org::org-headline-stars
          smug/parse/org::org-headline-body))


(defun <org-section/> (section)
  (let* ((body (org-section-body section))
         (title (org-section-headline section))
         (h? (concatenate 
              'string "h"
              (princ-to-string (1+ (min 6 (length (org-headline-stars title)))))))
         (id (unique-id section :id-hashes *unique-ids*)))
    (<> (div :class "org-section" :id id)
      (<> `(,h?) (<org-html> (org-headline-body title)))
      (<org-body/> body)
      (dolist (sec (org-section-sections section))
        (<org-section/> sec)))))
  
    


  (import 'yasexml:<>)
  
  (defmethod yasexml:call-with-tag (fn (tag (eql 'html)) &rest args)
    (apply #'<html> (lambda () (funcall fn tag)) args))
  
  (defun <html> (thunk &key (title "SMUG") 
                         (style "
  .header {
    
    top: 0;
    width: 100%;
    height:30%; overflow:scroll;
    max-height: 50%;
    background-color: #f5f5f5;
 }

ul.affix {
        position: fixed; 
        top: 0px;
        left: 62px;
        width: 250px; 
}
ul.affix-top {
        position: static;
}
ul.affix-bottom {
        position: absolute;
}

/* First level of nav */
.sidenav {
  margin-top: 30px;
  margin-bottom: 30px;
  padding-top:    10px;
  padding-bottom: 10px;
  background-color: #f7f5fa;
  border-radius: 5px;
}

/* All levels of nav */
.sidebar .nav > li > a {
  display: block;
  color: #716b7a;
  padding: 5px 20px;
}
.sidebar .nav > li > a:hover,
.sidebar .nav > li > a:focus {
  text-decoration: none;
  background-color: #e5e3e9;
}
.sidebar .nav > .active > a,
.sidebar .nav > .active:hover > a,
.sidebar .nav > .active:focus > a {
  font-weight: bold;
  color: #563d7c;
  background-color: transparent;
}

/* Nav: second level */
.sidebar .nav .nav {
        display: none;
}
.sidebar .nav > li.active .nav {
        display:block;
}
.sidebar .nav .nav {
  margin-bottom: 8px;
}
.sidebar .nav .nav > li > a {
  padding-top:    3px;
  padding-bottom: 3px;
  padding-left: 30px;
  font-size: 90%;
}
 .sidebar .nav > .active > a,
.sidebar .nav > .active:hover > a,
.sidebar .nav > .active:focus > a {
  font-weight: bold;
  color: #563d7c;
  background-color: transparent;
 "))
     (<> (:unescaped "<!DOCTYPE html>" #\Newline))
      (<> ("html" :lang "en")
        (<> head
          (<> title (<> (:text title)))
          (<> (:unescaped '|
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <!-- Latest compiled and minified CSS -->
  <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.0/css/bootstrap.min.css">
  
  <!-- Optional theme -->
  <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.0/css/bootstrap-theme.min.css">
   
  <!-- jQuery (necessary for Bootstrap's JavaScript plugins) -->
  <script src="http://cdnjs.cloudflare.com/ajax/libs/jquery/2.1.1/jquery.min.js"></script> 
  <!-- Latest compiled and minified JavaScript -->
  <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.0/js/bootstrap.min.js"></script>
  
   |))
          (<> style (<> (:text style))))
        (<> (body :data-spy "scroll" :data-target "#affix-nav") (funcall thunk))))
 

(defun <org-subsections-ul/> (subs
                              &key (class "nav sidenav")
                                   (attributes `(:data-spy "affix" :data-offset-top "10"))
                              &aux (unique-ids *unique-ids*))
  (when subs 
    (<> `(ul :class ,class ,@attributes)
      (dolist (sub subs)
        (<> li 
          (<> (a :href (concatenate 'string "#"
                                    (unique-id sub :id-hashes unique-ids)))
            (html (smug/parse/org::org-headline-body (org-section-headline sub))))
          (<org-subsections-ul/> (org-section-sections sub) 
                                 :class "nav" :attributes nil))))))
