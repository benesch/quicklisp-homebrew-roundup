;;;; quicklisp-homebrew-roundup.asd

(asdf:defsystem #:quicklisp-homebrew-roundup
  :serial t
  :description "Describe quicklisp-homebrew-roundup here"
  :author "Nikhil Benesch <nikhil.benesch@gmail.com>"
  :license "MIT"
  :depends-on (#:quicklisp
               #:ironclad)
  :components ((:file "package")
               (:file "quicklisp-homebrew-roundup")))

