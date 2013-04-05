#+quicklisp (quote #.(ql:quickload "alexandria"))

(defpackage :drewc.org/smug/parser/org
  (:documentation "Org Syntax Parser 
http://orgmode.org/worg/dev/org-syntax.html")
  (:import-from :drewc.org/smug/package
		#:<parser>
		#:is 
		#:is-not
		#:result
		#:maybe
		#:guard
		#:item)
  (:use :cl)
  (:export 
   #:read-org-file
   #:map-org
    #:org-file
    #:org-section

    #:org-src-block
    #:org-src-block-p
    #:org-src-block-name
    #:org-src-block-header-arguments
))
  
(in-package :drewc.org/smug/parser/org)



(defun maybep (parser1 &optional (parser2 (result nil)))
  (maybe parser1 parser2))

(defun parser-list (parser &rest parsers)
  (if parsers
      (smug:let* ((x parser)
		  (xs (apply #'parser-list parsers)))
	(result (cons x xs)))
      (smug:let* ((x parser))
	(result (list x)))))

(defun EOF () (smug:not (smug:item)))

(defun orp (parser &rest parsers)
  (if parsers
      (maybe parser (apply #'orp parsers))
      parser))

(defun string-equalp (string)
  (smug:string string :item-is #'char-equal))

(defun org-char ()
  (is-not #'char= #\Newline))

(defun org-word ()
  (smug:let* ((xs (smug:every 
		   (smug:progn (smug:not (is #'char= #\space))
			       (org-char)))))
    (result (coerce xs 'cl:string))))

(defun characters (&key (org-char (org-char)))
  (smug:some org-char))

(defun line (&key (characters (characters))
	       (eol (is #'char= #\Newline)))
  (smug:let* ((line (smug:prog1 characters			
		      eol)))
    (result (coerce line 'cl:string))))

(defstruct (org-heading (:type list))
  stars
  text)

(defun org-heading (&key (level 0))
  (smug:let* 
      ((stars (smug:prog1 (smug:every (is #'char= #\*))
		(is #'char= #\Space)))
       (text (smug:progn (guard #'> (length stars)
			   level) 
		    (line))))
    (result (make-org-heading
	     :stars stars
	     :text text))))


(defstruct org-src-block 
  (name nil)
  begin
  (language nil) 
  (switches nil)
  (header-arguments nil)
  contents
  end)

(defun org-src-block-name-heading ()
  (smug:let* ()
    (smug:progn (smug:string "#+name: " :item-is #'char-equal)
		(line))))

(defun org-src-block-switch ()
  (is #'char= #\-))

(defun org-src-block-header-argument-name ()
  (smug:prog2 (is #'char= #\:)
      (org-word)
    (is #'char= #\Space)))

(defun |org-src-block-header-argument (name . value)| ()
  (smug:let* ((name (org-src-block-header-argument-name))
	      (value (smug:some 
		      (smug:prog2 (smug:not (org-src-block-header-argument-name))
			  (org-word)
			(maybe (is #'char= #\Space)
			       (result nil))))))
    (result (cons name value))))

(defun org-src-block-header-arguments-as-alist ()
  (smug:some (|org-src-block-header-argument (name . value)|)))

(defstruct org-noweb-reference
  start 
  name
  end)

(defun org-noweb-reference ()
  (let ((end (parser-list (is #'char= #\>)
			  (is #'char= #\>))))
    (smug:let* ((start (parser-list (is #'char= #\<)
				    (is #'char= #\<)))
		(name (smug:every (smug:progn (smug:not end)
					      (org-char))))
		(end end))
      (result (make-org-noweb-reference 
	       :start start
	       :name (coerce name 'cl:string)
	       :end end)))))

(defun org-noweb-line ()
  (smug:let* 
      ((chars-or-refs (smug:prog1 
			  (smug:some
			   (maybe 
			    (org-noweb-reference)
			    (org-char)))
			(is #'char= #\Newline))))
    (let ((line (loop for cr in chars-or-refs
		   :if (characterp cr)
		   :collect cr :into chars
		   :else 
		   :nconc (prog1 (list (coerce chars 'cl:string)
				       cr)
			    (setf chars nil))
		   :into line		   		     
		   :finally (return (nconc line (list (coerce chars 'cl:string)))))))        
      (result (if (second line) line (first line))))))

(defun org-src-block-lines (&key end noweb)
   (smug:let* ((line (smug:progn 
		      (smug:not end)
		      (if noweb (org-noweb-line) (line))))
	       (lines (maybe (org-src-block-lines 
			      :end end
			      :noweb noweb)
			     (result nil))))
     (result (cons line lines))))

(defun org-src-block (&key 
			(begin (string-equalp "#+begin_src "))
			(end (string-equalp "#+end_src")))
  (smug:let* ((name (smug:maybe (org-src-block-name-heading)
				  (result nil)))
	      (begin begin)
	      (language (maybe (org-word)
			       (result nil)))
	      (_ (maybe (is #'char= #\space)
			(result nil)))
	      
	      (header-arguments 
	       (maybep
		(smug:let* ((alist (org-src-block-header-arguments-as-alist))
			    (rest (line)))
		  (result (if (and rest
				   (not (string= "" rest)))
			      
			      (list* `("rest of line" ,rest) alist)
			      alist)))
		(line)))
	      (contents (org-src-block-lines 
			 :end end
			 :noweb (assoc "noweb" header-arguments
				       :test #'string-equal)))
	      (end end))
      (result (make-org-src-block 
	       :name name
	       :begin begin
	       :language language 
	       :switches 'switches
	       :header-arguments header-arguments
	       :contents contents  
	       :end end))))

(defstruct org-section
  (heading NIL)
  (contents NIL))

(defun org-section (&key (level 0))
  (smug:progn 
    (smug:not (EOF))
    (smug:let* ((heading (if (zerop level)
			     (maybe (org-heading :level level)
				    (progn  (result (make-org-section))))
			     (org-heading :level level)))
		(contents  (smug:some 
			    (orp
			     (org-src-block)
			     (org-section :level (1+ level))
			     (smug:progn 
			       (smug:not (org-heading))
			       (line))))))
      (result (make-org-section 
	       :heading heading
	       :contents contents)))))

(defun map-org (function org-*)
  (cond ((org-section-p org-*) 
	 (funcall function org-*)
	 (dolist (c (org-section-contents org-*))
	   (map-org function c)))
	((org-file-p org-*) 
	 (funcall function org-*)
	 (dolist (c (org-file-contents org-*))
	   (map-org function c)))
	((listp org-*)
	 (dolist (object org-*)
	   (map-org function object)))
	(t (funcall function org-*))))

(defstruct org-file 
  pathname
  contents)

(defun org-file ()
  (smug:let* ((contents (smug:every (org-section))))
    (result (make-org-file 
	     :contents contents))))

(defun read-org-file (pathname)
  (let ((org-file 
	  (smug:run 
	   (org-file)
	   (alexandria:read-file-into-string  
	    pathname))))
    (prog1 org-file
      (setf (org-file-pathname org-file) pathname))))
