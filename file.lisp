(defpackage  :smug/file
  (:use :cl )
  (:import-from #:flexi-streams)

  (:export #:read-file-into-string))
  
(in-package :smug/file)

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
