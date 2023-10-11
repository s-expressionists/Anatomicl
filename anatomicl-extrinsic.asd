(cl:in-package #:asdf-user)

(defsystem #:anatomicl-extrinsic
  :description "Extrinsic interface to Anatomicl."
  :license "BSD"
  :author ("Robert Strandh"
           "Sylvia Harrington"
           "Tarn W. Burton")
  :maintainer "Tarn W. Burton"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Anatomicl"
  :bug-tracker "https://github.com/s-expressionists/Anatomicl/issues"
  :depends-on (#:anatomicl)
  :in-order-to ((asdf:test-op (asdf:test-op #:anatomicl-extrinsic/test)))
  :components ((:module code
                :pathname "code/extrinsic/"
                :serial t
                :components ((:file "packages")
                             (:file "interface")))))

(asdf:defsystem "anatomicl-extrinsic/test"
  :description "Extrinsic testing interface to Anatomicl."
  :license "BSD"
  :author ("Robert Strandh"
           "Sylvia Harrington"
           "Tarn W. Burton")
  :maintainer "Tarn W. Burton"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Anatomicl"
  :bug-tracker "https://github.com/s-expressionists/Anatomicl/issues"
  :depends-on ("ansi-test-harness")
  :perform (asdf:test-op (op c)
             (symbol-call :anatomicl-extrinsic/test :test))
  :components ((:module code
                :pathname "code/extrinsic/test/"
                :serial t
                :components ((:file "packages")
                             (:file "ansi-test")
                             (:static-file "expected-failures.sexp")))))
