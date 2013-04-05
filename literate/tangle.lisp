(defpackage :drewc.org/smug/literate/tangle
  (:documentation 
   "Literate Programming : Tangling using SMUGs org parser")

  (:use :cl)
  (:import-from :drewc.org/smug/parser/org
		#:org-file
		#:org-section
		#:map-org
		#:org-src-block
		#:org-src-block-p
		#:org-src-block-name
		#:org-src-block-header-arguments
		#:org-src-block-contents))
  
(in-package :drewc.org/smug/literate/tangle)

(defun org-src-blocks (org-file)
     (let ((ht (make-hash-table 
	       :test #'equalp)))
       (prog1 ht 
	 (map-org (lambda (org)
		    (when (org-src-block-p org)
		      (push org (gethash (org-src-block-name org) 
					 ht nil))))
		  org-file))))

(defun org-src-block-lines (block)
  (let (whitespace
	lines)
    (loop 
       (catch 'whitespace
	 (dolist (line (org-src-block-contents block))
	   (let* ((line-whitespace 
		   (subseq line 0 (position-if-not 
				   (lambda (char)
				     (char= char #\Space))
				   line)))
		  (output-line 
		   (subseq line 
			   (if (string= line "")
			       0
			       (length 
				(cond ((< (length line-whitespace)
					  (length whitespace))
				       line-whitespace)
				      (t (or whitespace
					     line-whitespace))))))))
	     (cond ((not whitespace)
		    (setf whitespace line-whitespace))     
		   ((and (not (string= output-line ""))
			 (> (length whitespace)
			    (length line-whitespace)))
		    (setf whitespace line-whitespace
			  lines nil)
		    (throw 'whitespace nil)))
	     (push output-line lines))))
       (return nil))
    (nreverse lines)))

(defun tangle (org-file)
  (let ((org-src-blocks 
	 (org-src-blocks org-file)))
    (maphash 
     (lambda (key blocks)
       (declare (ignorable key))
       (loop :for block :in blocks
	  :do (let ((header (org-src-block-header-arguments block)))		
		(when (and header (listp header))
		  (let ((tangle (assoc "tangle" header :test #'string-equal))
			(noweb (assoc "noweb" header :test #'string-equal))
			(padline (or (assoc "padline" header :test #'string-equal) 
				     t)))
		    
		    (when tangle 	
		      (break "~A ~% ~A" header (org-src-block-contents block))
		      ))))))
     org-src-blocks)))
