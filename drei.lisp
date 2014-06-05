(defpackage :smug/drei
  (:use :cl)
  (:import-from :smug/pure
		#:item-input)
  (:import-from :drei-buffer
		#:buffer
		#:mark))

(in-package :smug/drei)

(defmethod item-input (<p> (input-buffer buffer))
  `(,(cons (drei-buffer:buffer-object input-buffer 0)
	   (drei-buffer:make-buffer-mark input-buffer 1))))

(defmethod item-input (<p> (mark mark))
  (unless (drei-buffer:end-of-buffer-p mark)
    `(,(cons (drei-buffer:buffer-object 
	      (drei-buffer:buffer mark) 
	      (drei-buffer:offset mark))
	     (drei-buffer:make-buffer-mark 
	      (drei-buffer:buffer mark) 
	      (1+ (drei-buffer:offset mark)))))))






