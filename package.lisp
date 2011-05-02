;;;; package.lisp

(defpackage :smug
  (:use :cl)
  (:export 
   #:bind
   #:result
   #:parse 
   #:item
   
   #:=let*
   #:=char
   #:=and
   #:=or
   #:=not
   
   #:zero-or-more
   #:one-or-more
   
   #:no-more-input

   #:text
   #:whitespace
   #:none-of
   #:skip-whitespace
   
   #:run
   #:bracket
   #:maybe
   #:range
   #:at-least
   #:=string
   #:one-to
   #:zero-to
   #:=satisfies
   #:digit
   #:fail
   #:=unless
   #:line
   #:=prog1
   #:string-of
   #:=list
   #:call
   #:=digit-char
   #:exactly
   #:natural-number
   #:int
   #:=prog2))

(defpackage :smug.examples 
  (:use :cl :smug))

