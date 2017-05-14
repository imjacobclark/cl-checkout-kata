;;;; checkoutkata.asd

(asdf:defsystem #:checkoutkata
  :description "Describe checkoutkata here"
  :author "Jacob Clark"
  :license "LLGPL"
  :depends-on (#:prove)
  :serial t
  :components ((:file "package")
               (:file "checkoutkata")))

