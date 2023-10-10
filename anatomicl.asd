(cl:in-package #:asdf-user)

(defsystem #:anatomicl
  :description "Portable STRUCTURE implementation for Common Lisp."
  :license "BSD"
  :author ("Robert Strandh"
           "Sylvia Harrington"
           "Tarn W. Burton")
  :maintainer "Tarn W. Burton"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Anatomicl"
  :bug-tracker "https://github.com/s-expressionists/Anatomicl/issues"
  :depends-on (#:alexandria
               (:feature (:not :sicl) #:closer-mop))
  :components ((:module code
                :serial t
                :components ((:file "packages")
                             (:file "structure-slot-definition")
                             (:file "copy-structure")
                             (:file "read-structure")
                             (:file "print-structure")
                             (:file "defstruct-description-defclass")
                             (:file "defstruct-parse")
                             (:file "defstruct-support")
                             (:file "defstruct-expand-object")
                             (:file "defstruct-expand-typed")
                             (:file "conditions")
                             (:file "interface")))))
