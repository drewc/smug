(defpackage :smug/weave/html
  (:use :cl :smug/tutorial :smug/parse/org)

  (:import-from :smug/parse/outline
                #:.outline)
  (:import-from :smug/parse/org
                #:org-file
                #:setting
                #:setting-name
                #:setting-words 
                #:footnote-reference)
  (:import-from :alexandria)
  (:import-from :split-sequence)
  (:import-from :yasexml
                #:<>)
  (:import-from :closure-html)
  (:export))
(in-package :smug/weave/html) 


(defun loop-and-concatenate (list-or-thing &optional return)
 (if (not (listp list-or-thing))
     list-or-thing
     (let (text body)
       (flet ((push-text () 
                (when text                        
                  (push (apply #'concatenate 'string 
                               (mapcar #'string (nreverse text)))
                        body)
                  (setf text nil))))
         (let ((result
                (loop :for thing :in list-or-thing
                   :do
                   (etypecase thing
                     ((or character string)
                      (push thing text))
                     (list 
                      (push-text)
                      (dolist (i (loop-and-concatenate thing t))
                        (etypecase i
                          ((or character string)
                           (push i text))
                          (t 
                           (push-text)
                           (push i body)))))
                     (t (push-text)
                        (push thing body)))
                   :finally 
                   (push-text) (return (nreverse body)))))
           (cond (return result)
                 ((rest result) (loop-and-concatenate result t))
                 (t (first result))  ))))))
                     
  
  
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
    (remove-if (lambda (i) (find i " ~:+#.!|<>\"()'*,?/`"))
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
      
  

(defun <org-<p>s> (body)
  (<> div 
    (let ((things (loop for item in (if (listp body) body (list body))
               :collect (typecase item 
                          ((or string markup footnote-reference)
                           (<> (:handler (chtml:make-string-sink))
                             (<org-html> item)))
                          (t item))
               :into list
                   :finally (return (loop-and-concatenate list t)))))
    (loop for thing in things
       :collect (if (stringp thing)
                    (let* ((list (split-sequence:split-sequence #\Newline thing))
                           (ps (split-sequence:split-sequence "" list :test #'string=)))
                      (dolist (p ps) (when p (<> p (<> `(:unescaped ,@p))))))
                    (<org-html> thing))))))  

(defun princ-org-to-string (org)
  (with-output-to-string (s)
    (princ-org org s)))

(defgeneric princ-org (org &optional stream)
  (:method (thing &optional stream)
    (princ thing stream))
  (:method ((list list) &optional stream)
    (dolist (i list) (princ-org i stream))))

(defmethod princ-org ((setting setting) &optional stream)
  (when *print-readably*
    (princ "#+" stream) (princ (setting-name setting) stream)
    (princ ": " stream) (princ (setting-words setting) stream)))

(import '(smug/parse/org::footnote-reference
          smug/parse/org::footnote-reference-marker))

(defmethod princ-org ((footnote-reference footnote-reference) 
                      &optional stream)
  (princ-org (list #\[
                   (footnote-reference-marker footnote-reference)
                   #\]) stream))

(import '(smug/parse/org::markup
          smug/parse/org::markup-text
          smug/parse/org::markup-char))

(defmethod princ-org ((markup markup)
                      &optional stream)
  (princ-org (list (markup-char markup)
                   (markup-text markup)
                   (markup-char markup)) 
             stream))
            

(defgeneric <org-html> (org &key &allow-other-keys)
  (:method (thing &key &allow-other-keys)
    (<> (:text (princ-org-to-string thing))))
  (:method ((list list) &key &allow-other-keys)
    (dolist (thing list)
      (<org-html> thing))))

(defmethod <org-html> ((setting setting) &key &allow-other-keys)
  (<> (:comment (let ((*print-readably* t))
                  (princ-org-to-string setting)))))

(import '(smug/parse/org::footnote-reference
          smug/parse/org::footnote-reference-marker))

(defmethod <org-html> ((footnote-reference footnote-reference) 
                       &key &allow-other-keys)
  (<> (a :href "#")
    (call-next-method)))

(import '(smug/parse/org::markup
          smug/parse/org::markup-text
          smug/parse/org::markup-style))

(defmethod <org-html> ((markup markup)
                       &key &allow-other-keys)
  (<> `(,(markup-style markup))
    (<> (:text (markup-text markup)))))

(import '(smug/parse/org::blockquote
          smug/parse/org::blockquote-text))

(defmethod <org-html> ((blockquote blockquote) 
                       &key &allow-other-keys)
  (<> (blockquote) 
    (<> (:text (blockquote-text blockquote)))))

(import '(smug/parse/org::code-block
          smug/parse/org::code-block-body
           smug/parse/org::code-block-language
          ))

(defmethod <org-html> ((code-block code-block) 
                       &key &allow-other-keys)
  (<> (pre)
    (<> (code :class (code-block-language code-block))
    (dolist (b (code-block-body code-block))      
      (<> (:text b))))))
            

  
(import '(smug/parse/org::org-document-body
          smug/parse/org::org-document
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
         (length (1+ (min 6 (length (org-headline-stars title)))))
         (h? (concatenate 
              'string "h"
              (princ-to-string length)))
         (id (unique-id section :id-hashes *unique-ids*)))
    (<> (div :class "org-section" :id id)
      (<> `(,h? ,@(when (>= length 2)
                    (list :class "page-header")))
        (<org-html> (org-headline-body title)))
      (<org-body/> body)
      (dolist (sec (org-section-sections section))
        (<org-section/> sec)))))
  
    


  (import 'yasexml:<>)
  
  (let ((highlight 
  "HighlightLisp.highlight_drewc = HighlightLisp.highlight_element;
  
  HighlightLisp.highlight_element = function(code_el)
   {  
    this.highlight_drewc(code_el);
    var html = code_el.innerHTML;
    html = html.replace(/<(?!\\/span>|span(.*)>)/g, '&lt;');
    // html = html.replace(/(?!span)>/g, '&amp;');
    /* html = html.replace(/(?!<span)>/g, '&gt;'); */
   //     console.log(html);
    code_el.innerHTML = html;
   };

function isElementVisible(elementToBeChecked)
{
    var TopView = $(window).scrollTop();
    var BotView = TopView + $(window).height();
    var TopElement = $(elementToBeChecked).offset().top;
    var BotElement = TopElement + $(elementToBeChecked).height();
    return ((BotElement <= BotView) && (TopElement >= TopView));
}

function make_it_so(thing)
{
  var li_for_nav = thing.target;
  var nav = thing.currentTarget;

  if (!isElementVisible(li_for_nav)) 
  {
   /* var href=$( li_for_nav ).find('a')[0].hash;
   var section = $(href)[0];

   var link_top = $( li_for_nav ).offset().top;
   var section_top = $( section ).offset().top;
   var nav_top = $( nav ).offset().top 

   var new_nav_top = (nav_top + $(window).height()) - section_top;
   
   $( nav ).css('position', 'fixed');
   $( nav ).css('top', '50px'); */
   
     


  
  } else { 
  console.log('active')
  }
}

$('#affix-nav').on('activate.bs.scrollspy', function (event) {
 // console.log(event);  
 make_it_so(event);

})



"))
    (defun <script-highlight-body/> ()
    (<> (script) (<> (:text highlight)))
      (<> (script) (<> (:text "HighlightLisp.highlight_auto();")))))
  
  
  
  
(import 'yasexml:<>)
  
(defmethod yasexml:call-with-tag (fn (tag (eql 'html)) &rest args)
  (apply #'<html/> (lambda () (funcall fn tag)) args))


(defun <html/> (thunk &key (title "SMUG") 
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
       /* position: static; */
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
  <script type="text/javascript" src="http://orthecreedence.github.io/highlight-lisp/js/highlight-lisp/highlight-lisp.js"></script>
  <link rel="stylesheet" id="hl-theme" href="http://orthecreedence.github.io/highlight-lisp/js/highlight-lisp/themes/github.css">
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
    (<> (body :data-spy "scroll" :data-target "#affix-nav")
      (funcall thunk)
      (<script-highlight-body/>)
      )))

 

(defun <org-subsections-ul/> (subs
                              &key (class "nav sidenav")
                                   (attributes `(:data-spy "affix" :data-offset-top "0"
                                                           :data-offset-bottom "1000"))
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
