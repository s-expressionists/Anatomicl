(cl:in-package #:asdf-user)

(defsystem #:anatomicl-extrinsic
  :depends-on (#:anatomicl)
  :components ((:module code
                :pathname "code/extrinsic/"
                :serial t
                :components ((:file "packages")
                             (:file "interface")))))
