(cl:in-package #:common-lisp-user)

(defpackage #:anatomicl
  (:use #:common-lisp)
  (:local-nicknames (#:mop #+sicl #:sicl-clos #-sicl #:closer-mop))
  (:shadow #:copy-structure)
  (:export #:client-form
           #:copy-structure
           #:define-interface
           #:expand-defstruct
           #:parse-defstruct
           #:print-structure
           #:structure-class-name
           #:structure-description
           #:structure-direct-slot-definition
           #:structure-effective-slot-definition
           #:structure-object-name
           #:structure-slot-definition
           #:structure-slot-definition-read-only))
