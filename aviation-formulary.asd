;;;; aviation-formulary.asd

(asdf:defsystem #:aviation-formulary
  :description "A Common Lisp library implementing the algorithms found in the Aviation Formulary."
  :author "Jeff Francis <jeff@gritch.org>"
  :license "MIT, see file LICENSE"
  :serial t
  :depends-on (#:local-time
               #:bordeaux-threads)
  :components ((:file "package")
               (:file "aviation-formulary")))

