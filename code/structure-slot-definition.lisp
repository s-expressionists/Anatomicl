(cl:in-package #:anatomicl)

;;; We inherit from sb-pcl::structure-slot-definition so that the slot names can
;;; be constant.
(defclass structure-slot-definition (closer-mop:standard-slot-definition
                                     #+sbcl sb-pcl::structure-slot-definition)
  ((%read-only :initarg :read-only :reader structure-slot-definition-read-only)))

(defclass structure-direct-slot-definition (structure-slot-definition
                                            closer-mop:standard-direct-slot-definition)
  ())

(defclass structure-effective-slot-definition (structure-slot-definition
                                               closer-mop:standard-effective-slot-definition)
  ())
