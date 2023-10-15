(cl:in-package #:anatomicl)

;;; We inherit from sb-pcl::structure-slot-definition so that the slot names can
;;; be constant.
(defclass structure-slot-definition (closer-mop:standard-slot-definition
                                     #+sbcl sb-pcl::structure-slot-definition)
  ((%read-only :initarg :read-only :accessor structure-slot-definition-read-only)))

#+abcl
(defmethod initialize-instance :after ((instance structure-slot-definition) &rest initargs &key read-only)
  (declare (ignore initargs))
  (setf (structure-slot-definition-read-only instance) read-only))

(defclass structure-direct-slot-definition (structure-slot-definition
                                            closer-mop:standard-direct-slot-definition)
  ())

(defclass structure-effective-slot-definition (structure-slot-definition
                                               closer-mop:standard-effective-slot-definition)
  ())
