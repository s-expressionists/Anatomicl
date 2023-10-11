(cl:in-package #:asdf-user)

(defsystem #:anatomicl-intrinsic
  :description "Intrinsic interface to Anatomicl."
  :license "BSD"
  :author ("Robert Strandh"
           "Sylvia Harrington"
           "Tarn W. Burton")
  :maintainer "Tarn W. Burton"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Anatomicl"
  :bug-tracker "https://github.com/s-expressionists/Anatomicl/issues"
  :depends-on (#:anatomicl)
  :components ((:module code
                :pathname "code/intrinsic/"
                :serial t
                :components ((:file "packages")
                             (:file "interface")))))
