#+quicklisp '#.(ql:quickload '("flexi-streams"))
(defpackage :smug/literate/tangle
  (:documentation 
   "Literate Programming : Tangling using SMUGs org parser")
  (:use :cl :smug/pure/dot)
  (:import-from :smug/parser/org 
		#:.greater-block
		#:.greater-element
		#:.line
		#:source-block
		#:source-block-arguments
		#:element-block-contents
		#:greater-element-keywords)

  (:export #:source-blocks
	   #:tangle-file))
  
(in-package :smug/literate/tangle)

(defstruct noweb-reference start name end)
	   
(defun .noweb-reference ()
  (flet ((.nw ()
	   (.prog2 
	    (.string "<<")
	    (.every (.or (.is #'alphanumericp)
			 (.is #'char= #\-)))
	    (.string ">>"))))
    (.let* ((start (.some (.progn (.not (.nw)) 
				  (.item))))
	    (ref (.coerce (.nw) 'string))
	    (end (.some (.item))))
      (.result (make-noweb-reference 
		:start start 
		:name ref
		:end end)))))

(defun read-file-into-string (pathname)
  (with-open-file (stream pathname :element-type '(unsigned-byte 8))
    (let ((input (flexi-streams:make-flexi-stream 
		  stream :external-format :utf8))
	  (buffer-size 4096))
      (let ((*print-pretty* nil))
	(with-output-to-string (datum)
	  (let ((buffer (make-array buffer-size :element-type 'character)))
	    (loop
	       :for bytes-read = (read-sequence buffer input)
	       :do (write-sequence buffer datum :start 0 :end bytes-read)
	       :while (= bytes-read buffer-size))))))))

(defun source-blocks (pathname)
  (remove-if-not (lambda (i) (typep i 'source-block)) 
		 (.run (.every (.or (.greater-element (.greater-block)) 
				    (.line)))
		       (read-file-into-string pathname))))

(defun source-block-plist (source-block)
  (let ((sba (smug/parser/org::source-block-arguments source-block))
	(*readtable* (copy-readtable *readtable*)))
    (read-from-string (concatenate 'string "(" sba ")"))))

(defun source-block-alist (source-block)
  (let ((sba (smug/parser/org::source-block-arguments source-block))
	(*readtable* (copy-readtable *readtable*)))
    (loop for (n v) 
       on (read-from-string (concatenate 'string "(" sba ")"))
	 by #'cddr
	 collect (cons n v))))

(defun source-block-tangle (sb)
  (getf (source-block-plist sb) :tangle))

(defun source-block-noweb (sb)
  (cdr (assoc :noweb (source-block-alist sb))))

(defun source-block-padline (sb)
  (or (cdr (assoc :padline (source-block-alist sb)))
      "yes"))

(defun source-block-name  (sb)
  (let ((name (find "name" (greater-element-keywords sb)
		    :key #'smug/parser/org::keyword-name
		    :test #'string-equal)))
    (and name (smug/parser/org::keyword-value name))))

(defun source-block-contents (sb)
  (element-block-contents sb))

(defun remove-min-space-prefix (lines)
  (let* ((contents lines)
	 (left-trim 
	  (apply 
	   #'min
	   (remove nil (loop for line in contents
			  :collect (unless (string= "" line)
				     (position-if-not (lambda (char)
							(char= char #\Space)) 
						      line)))))))
    (loop for c in contents
       :collect (if (<= left-trim (length c))
		    (subseq c left-trim)
		    c))))

(defun merge-noweb-references (lines source-blocks)
  "=> New /lines/, a list of strings"
  (loop :for line in lines 
     :for noweb = (.run (.noweb-reference) line)
     :append (if (not noweb) 
		 (list line)
		 (let* ((start (noweb-reference-start noweb))
			(block (find (noweb-reference-name noweb) source-blocks 
				     :key #'source-block-name
				     :test #'string-equal)))
		   (loop :for (other-line . rest)
		      :on (source-block-contents block)
		      :collect (concatenate 
				'string start other-line 
				(unless rest (noweb-reference-end noweb))))))))
  
  
(defun tangle-source-block (source-block other-source-blocks)
  (let ((tangle (source-block-tangle source-block))
	(noweb (source-block-noweb source-block))
	(lines (source-block-contents source-block))
	(padline (source-block-padline source-block)))
    (when (string-equal noweb "yes")
      (setf lines (merge-noweb-references lines other-source-blocks)))
    (cond ((string-equal "yes" tangle))
	  (t (with-open-file (stream (merge-pathnames tangle)
				:direction :output
				:if-exists :supersede)
	       (loop :for c 
		  :in (remove-min-space-prefix 
		       lines)
		  :do (princ c stream)			   
		  (terpri stream)))))
    (when tangle 
      (list padline tangle (read-file-into-string (pathname (string tangle)))))))

(defun tangle (pathname)
  (let* ((*default-pathname-defaults* pathname)
	 (sbs (source-blocks pathname))
	 (tangles (remove-if-not #'source-block-tangle
				 sbs)))
    (mapcar #'tangle-source-block sbs)))

(defun tangle-file (pathname)
  (let* ((*default-pathname-defaults* pathname)
	 (sbs (source-blocks pathname))
	 (tangles (remove-if-not #'source-block-tangle
				 sbs)))    
    (loop for tangle in tangles 
       :collect (tangle-source-block tangle sbs))))


(defparameter %p 
  #P"/me/src/smug/parser/org-syntax.org")







  









