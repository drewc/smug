;;;; smug.asd

(asdf:defsystem #:smug
  :serial t
  :components ((:file "package")
	       (:file "input")
               (:file "smug")))

