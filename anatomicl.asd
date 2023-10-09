(cl:in-package #:asdf-user)

(defsystem #:anatomicl
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
