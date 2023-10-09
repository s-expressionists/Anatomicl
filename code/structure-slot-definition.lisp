(cl:in-package #:anatomicl)

(defclass structure-slot-definition (mop:standard-slot-definition)
  ((%read-only :initarg :read-only :reader structure-slot-definition-read-only)))

(defclass structure-direct-slot-definition (structure-slot-definition
                                            mop:standard-direct-slot-definition)
  ())

(defclass structure-effective-slot-definition (structure-slot-definition
                                               mop:standard-effective-slot-definition)
  ())
