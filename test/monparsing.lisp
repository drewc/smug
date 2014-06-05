
(defpackage :smug/test/monparsing
  (:use :cl)
  (:shadow #:sequence
           #:satisfies
           #:char)
  (:export #:sequence))
  
(in-package :smug/test/monparsing)



(assert (equal '((1 . "asd"))
               (funcall (result 1) "asd")))


 
(assert (eq NIL (funcall (zero) "asd")))



(assert (equal '((#\a . "sd"))
               (funcall (item) "asd")))

(assert (equal '((#\a . ""))
               (funcall (item) "a")))

(assert (null
         (funcall (item) "")))
     
(defpackage :smug/test/monparsing%sequence
  (:use :cl)
  (:shadow #:sequence)
  (:export #:sequence))

(in-package :smug/test/monparsing%sequence)
  
<<lisp by itself sequence >>

(in-package :smug/test/monparsing)

(flet ((sequence (p1 p2)
         (smug/test/monparsing%sequence:sequence 
          p1 p2)))

  (assert 
   (equal '(((#\a . #\s) . "d"))
          (funcall (sequence (item) (item)) "asd")))
  
  
  
  (labels ((f (&rest args) (apply #'cl:list args))
           (|lisp test bind| (p1 p2 pn)
            (bind 
             p1 
             (lambda (x1)
               (bind 
                p2 
                (lambda (x2) 
                  #| ... |#
                  (bind 
                   pn
                   (lambda (xn)
                     (result (f  x1 x2 #|...|# xn))))))))
             
           ))

     (let* ((p1 (item))
            (p2 (result 'yay!))
            (pn (sequence 
                 (item) (result 'two-much-yay!))))
       (assert (equal '(((#\A YAY! 
                          (#\S . TWO-MUCH-YAY!)) 
                         . "D"))
                      (funcall (|lisp test bind|
                                p1 p2 pn) "ASD"))))      

  ))

(defun sequence (parser-1 parser-2)
  (bind parser-1 
        (lambda (x)
     (bind parser-2 
           (lambda (y) 
             (result (cons x y)))))))

(assert 
 (equal '(((#\a . #\s) . "d"))
        (funcall (sequence (item) (item)) "asd")))

(labels ((f (&rest args) (apply #'cl:list args))
           (|lisp test bind| (p1 p2 pn)
            (bind 
             p1 
             (lambda (x1)
               (bind 
                p2 
                (lambda (x2) 
                  #| ... |#
                  (bind 
                   pn
                   (lambda (xn)
                     (result (f  x1 x2 #|...|# xn))))))))
             
           ))

     (let* ((p1 (item))
            (p2 (result 'yay!))
            (pn (sequence 
                 (item) (result 'two-much-yay!))))
       (assert (equal '(((#\A YAY! 
                          (#\S . TWO-MUCH-YAY!)) 
                         . "D"))
                      (funcall (|lisp test bind|
                                p1 p2 pn) "ASD"))))      

  )

(defun satisfies (predicate)
  (bind (item) 
        (lambda (x)
          (if (funcall predicate x)
              (result x)
              (zero)))))


(defun char (x)
 (satisfies (lambda (y) (char= x y))))

(defun digit ()
  (satisfies (lambda (x) 
               (and (char<= #\0 x)
                    (char>= #\9 x)))))

(defun lower ()
  (satisfies (lambda (x) 
               (and (char<= #\a x)
                    (char>= #\z x)))))

(defun upper ()
  (satisfies (lambda (x) 
               (and (char<= #\A x)
                    (char>= #\Z x)))))


(assert (equal '((#\H . "ello"))
          (funcall (upper) "Hello")))

(assert (cl:null 
          (funcall (lower) "Hello")))

(flet ((|lisp test string of length two| ()   
         (bind 
          (lower) 
          (lambda (x) 
            (bind 
             (lower) 
             (lambda (y) 
               (result (coerce (list x y) 'string))))))    
         ))
 
  (assert (equal '(("ab" . "cd"))
                 (funcall (|lisp test string of length two|) 
                          "abcd"))) 
  (assert (cl:null (funcall (|lisp test string of length two|) 
                            "aBcd")))
 )
  
(defun plus (parser qarser)
  (lambda (input) 
    (append (funcall parser input)
            (funcall qarser input))))

(defun letter () (plus (lower) (upper)))
(defun alphanum () (plus (letter) (digit)))
  
(defun word ()
  (flet ((ne-word ()
          (bind 
          (letter) 
          (lambda (x) 
          (bind (word) (lambda (xs) (result (cons x xs))))))))
  (plus (ne-word) (result nil)))) 


(assert (equal (funcall (word) "Yes!")
                  '(((#\Y #\e #\s) . "!") 
                    ((#\Y #\e) . "s!") 
                    ((#\Y) . "es!")
                    (NIL . "Yes!"))))
