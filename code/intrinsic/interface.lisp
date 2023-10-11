(cl:in-package #:anatomicl-intrinsic)

(defclass intrinsic-client () ())

(defvar *client* (make-instance 'intrinsic-client))

#-sicl
(progn
  (defvar *structure-descriptions* (make-hash-table :test #'eq))

  (defmethod anatomicl:structure-description ((client intrinsic-client) name environment)
    (declare (ignore environment))
    (values (gethash name *structure-descriptions*)))

  (defmethod (setf anatomicl:structure-description) (new-value (client intrinsic-client) name environment)
    (declare (ignore environment))
    (cond ((null new-value)
           (remhash name *structure-descriptions*))
          (t
           (setf (gethash name *structure-descriptions*) new-value)))))

#+sicl
(let* ((environment (env:global-environment))
       (global-environment-function
         (env:fdefinition
          sicl-client:*client* environment 'env:global-environment))
       (structure-description-function
         (env:fdefinition
          sicl-client:*client* environment 'env:structure-description))
       (setf-structure-description-function
         (env:fdefinition
          sicl-client:*client* environment '(setf env:structure-description))))

  (defmethod anatomicl:structure-description ((client intrinsic-client) name environment)
    (funcall structure-description-function
             name (funcall global-environment-function environment)))

  (defmethod (setf anatomicl:structure-description) (new-value (client intrinsic-client) name environment)
    (funcall setf-structure-description-function
             new-value name (funcall global-environment-function environment))))

(anatomicl:define-interface *client* intrinsic-client
  :intrinsic t
  #+sicl :structure-class-superclasses #+sicl '(sicl-clos:regular-class))
