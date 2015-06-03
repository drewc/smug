#-asdf3 (error "SMUG requires ASDF 3 or later with \"asdf-package-system\"")
#+quicklisp (ql:quickload "asdf-package-system")
		       
(asdf:defsystem :smug
  :description "SMUG: Simple Monadic Uber Go-into, Parsing made easy."
  :author "Drew Crampsie"
  :license "MIT"
  :long-description
  "Smug is a library for parsing text, based on _monadic parser
combinators_. 

Using a simple technique from the functional programming camp, smug
makes it simple to create quick extensible recursive descent parsers
without funky syntax or impenetrable macrology."
  :class :package-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (:smug/smug))


