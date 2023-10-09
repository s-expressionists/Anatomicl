(cl:in-package #:common-lisp-user)

(defpackage #:anatomicl-extrinsic
  (:use #:common-lisp)
  (:local-nicknames (#:mop #:closer-mop))
  (:shadow #:defstruct
           #:copy-structure
           #:structure-class
           #:structure-object)
  (:export #:defstruct
           #:copy-structure
           #:structure-class
           #:structure-object))
