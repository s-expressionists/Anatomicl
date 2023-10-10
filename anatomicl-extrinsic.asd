(cl:in-package #:asdf-user)

(defsystem #:anatomicl-extrinsic
  :depends-on (#:anatomicl)
  :in-order-to ((asdf:test-op (asdf:test-op #:anatomicl-extrinsic/test)))
  :components ((:module code
                :pathname "code/extrinsic/"
                :serial t
                :components ((:file "packages")
                             (:file "interface")))))

(asdf:defsystem "anatomicl-extrinsic/test"
  :depends-on ("alexandria"
               "anatomicl-extrinsic")
  :perform (asdf:test-op (op c)
             (symbol-call :anatomicl-extrinsic/test :ansi-test))
  :components ((:module code
                :pathname "code/extrinsic/test/"
                :serial t
                :components ((:file "packages")
                             (:file "ansi-test")))))
