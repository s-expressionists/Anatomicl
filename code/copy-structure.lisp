(cl:in-package #:anatomicl)

(defun copy-structure (client object)
  (let ((structure-object-name (structure-object-name client)))
    (assert (typep object structure-object-name) (object)
            'type-error :datum object :expected-type structure-object-name))
  (let* ((class (class-of object))
         (new-object (allocate-instance class)))
    (dolist (slot (closer-mop:class-slots class))
      ;; Use s-v-u-c here to avoid the extra slot lookup.
      (when (closer-mop:slot-boundp-using-class class object slot)
        (setf (closer-mop:slot-value-using-class class new-object slot)
              (closer-mop:slot-value-using-class class object slot))))
    new-object))
