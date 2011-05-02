(in-package :smug)

(defgeneric input-empty-p (input)  
  (:method ((input cl:string))
    (zerop (length input))))

(defgeneric input-first (input)
  (:method ((input cl:string))
    (declare (optimize (speed 3) (safety 0)))   
    (aref input 0)))

(defgeneric input-rest (input)
  (:method ((input cl:string))
    (declare (optimize (speed 3)))
    (multiple-value-bind (string index)
	(array-displacement input)
      (let ((original-string (or string input)))
    (make-array (1- (length input))
		:displaced-to original-string
		:displaced-index-offset (1+ index)
		:element-type 'character)))))

(defgeneric input-error (input message &rest args)
  (:method (input message &rest args) (error message args)))

(define-condition input-error (simple-error) ((input :accessor input-error-input :initarg :input)))

(defstruct (file-stream-input (:constructor %make-file-input))
  stream (file-position 0))

(defmethod input-first (input &aux (stream (file-stream-input-stream input)))
  (file-position stream (file-stream-input-file-position input))
  (read-char stream))

(defmethod input-rest (input) 
  (%make-file-input :stream (file-stream-input-stream input)
		    :file-position (1+ (file-stream-input-file-position input))))

(defmethod input-empty-p (input)
  (= (file-stream-input-file-position input) (file-length (file-stream-input-stream input))))

(defun make-file-stream-input (stream)
  (%make-file-input :stream stream))

(defmethod input-error ((input file-stream-input) message &rest args)
  (error 'input-error 
	 :input input
	 :format-control "Error at ~A : ~A" 
	 :format-arguments (list (file-stream-input-file-position input) 
				 (apply #'format nil message args))))
  
(defstruct (line-input (:constructor %make-line-input)) 
  string (index 0) (line-count 1))

(defun make-line-input (string)
  (%make-line-input :string string))

(defmethod input-empty-p ((input line-input))
  (= (line-input-index input)
     (1- (length (line-input-string input)))))

(defmethod input-first ((input line-input))
  (aref (line-input-string input) 
       (line-input-index input)))

(defmethod input-rest  ((input line-input))
  (%make-line-input :index (1+ (line-input-index input))
		    :string (line-input-string input)
		    :line-count (line-input-line-count input)))

(define-condition input-error (simple-error) ((input :accessor input-error-input :initarg :input)))

(defmethod input-error ((input line-input) message &rest args)
  (error 'input-error 
	 :input input
	 :format-control "Error on line ~A : ~A" 
	 :format-arguments (list (line-input-line-count input) (apply #'format nil message args))))

(defun fail (&key (error nil))
  (if error 
      (lambda (input)
	(input-error input error)
	(funcall (result t) input))
      (constantly nil)))
	 
(defun increment-line-count (&optional (delta 1))
  (lambda (input) 
    (funcall (result (incf (line-input-line-count input) delta))
	     input)))

