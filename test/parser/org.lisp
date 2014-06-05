(defpackage :drewc.org/smug/test/parser/org
  (:import-from :drewc.org/smug/parser/org
		#:org-section
		#:map-org
		#:org-src-block)
  (:use :cl))
  
(in-package :drewc.org/smug/test/parser/org)

(defparameter *org-string-test-1* 
  "* This is level 1
** This is level 2
*** This is level 3
**** This is level 4
* This level 1 (2) 
** This level 2 (2) 
* This is level 1 (3)
This is text under level 1
*This is also text under level 1

** This is level 2 (3)

Text under level 2
   Indented text

      Indented text 2

*** Level 3 (3) 

* Level 1 (4)

text text text
#+name: asd
#+begin_src lisp

asd

#+end_src


")





