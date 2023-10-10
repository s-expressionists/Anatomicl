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
  :serial t
  :components
  ((:file "packages")
   (:file "structure-class-defclass")
   (:file "structure-slot-definition")
   (:file "structure-object-defclass")
   (:file "find-structure-description")
   (:file "copy-structure")
   (:file "read-structure")
   (:file "print-structure")
   (:file "structure-object-print-object")
   (:file "defstruct-description-defclass")
   (:file "defstruct-parse")
   (:file "defstruct-support")
   (:file "defstruct-expand-object")
   (:file "defstruct-expand-typed")
   (:file "defstruct-defmacro")
   (:file "conditions")))
