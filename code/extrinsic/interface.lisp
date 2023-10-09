(cl:in-package #:anatomicl-extrinsic)

(defclass extrinsic-client () ())

(defvar *client* (make-instance 'extrinsic-client))

(defvar *structure-descriptions* (make-hash-table :test #'eq))

(defmethod anatomicl:structure-description ((client extrinsic-client) name environment)
  (declare (ignore environment))
  (values (gethash name *structure-descriptions*)))

(defmethod (setf anatomicl:structure-description) (new-value (client extrinsic-client) name environment)
  (declare (ignore environment))
  (cond ((null new-value)
         (remhash name *structure-descriptions*))
        (t
         (setf (gethash name *structure-descriptions*) new-value))))

(anatomicl:define-interface *client* extrinsic-client)
