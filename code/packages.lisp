(cl:in-package #:common-lisp-user)

(defpackage #:anatomicl
  (:use #:common-lisp)
  #+sicl (:local-nicknames (#:closer-mop #:sicl-clos))
  (:shadow #:find-class #:copy-structure)
  (:export #:client-form
           #:copy-structure
           #:define-interface
           #:expand-defstruct
           #:find-class
           #:parse-defstruct
           #:print-structure
           #:standard-constructor-p
           #:structure-class-name
           #:structure-description
           #:structure-direct-slot-definition
           #:structure-effective-slot-definition
           #:structure-object-name
           #:structure-slot-definition
           #:structure-slot-definition-read-only))
