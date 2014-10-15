#-asdf3 (error "SMUG requires ASDF 3 or later with \"asdf-package-system\"")
#+quicklisp (ql:quickload "asdf-package-system")
		       
(asdf:defsystem :smug
  :description "SMUG: Simple Monads Uber Good"
  :long-description
  "Smug is a library for parsing text, based on monadic parser
combinators [fn:1]. 

Using a simple technique from the functional programming camp, smug
makes it simple to create quick extensible recursive descent parsers
without funky syntax or impenetrable macrology."
  :class :package-system
  :defsystem-depends-on (:asdf-package-system))
