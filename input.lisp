(defpackage :drewc.org/smug/input
  (:nicknames :smug/input)
  (:use :cl)
  (:import-from :interface)
  (:export #:<input>
	   #:input-empty-p
	   #:input-first
	   #:input-rest))

(in-package :drewc.org/smug/input)

(interface:define-interface <input> (interface:<type>)
  () (:singleton) 
  (:generic input-empty-p (<input> input))
  (:generic input-first (<input> input))
  (:generic input-rest (<input> input))

  ;; CL:STRING 
  (:method> input-empty-p ((input cl:string))
    (zerop (length input)))

  (:method> input-first ((input cl:string))   
    (aref input 0))

  (:method> input-rest ((input cl:string))
    (multiple-value-bind (string index)
        (array-displacement input)
      (let ((original-string (or string input)))
        (make-array (1- (length input))
                    :displaced-to original-string
                    :displaced-index-offset (1+ index)
                    :element-type 'character)))))
