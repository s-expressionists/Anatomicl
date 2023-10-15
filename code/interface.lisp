(cl:in-package #:anatomicl)

(defmacro define-interface (client-var client-class &optional intrinsic)
  (let* ((pkg (if intrinsic (find-package "COMMON-LISP") *package*))
         (defstruct-name (intern "DEFSTRUCT" pkg))
         (copy-structure-name (intern "COPY-STRUCTURE" pkg))
         (structure-class-name (intern "STRUCTURE-CLASS" pkg))
         (structure-object-name (intern "STRUCTURE-OBJECT" pkg)))
    `(progn
       (defmethod client-form ((client ,client-class))
         ',client-var)

       (defmacro ,defstruct-name (&environment environment name-and-options &rest slots)
         (expand-defstruct ,client-var
                           (parse-defstruct name-and-options slots)
                           environment))

       (defun ,copy-structure-name (object)
         (copy-structure ,client-var object))

       (defmethod structure-class-name ((client ,client-class))
         ',structure-class-name)

       (defclass ,structure-class-name (#+sicl sicl-clos:regular-class
                                        #-sicl standard-class)
         ((%standard-constructor :initarg :standard-constructor
                                 :initform nil
                                 :reader standard-constructor-p)))

       (defmethod closer-mop:validate-superclass ((class ,structure-class-name) (superclass (eql (find-class 't))))
         ;; T is not a valid direct superclass, all structures inherit from STRUCTURE-OBJECT.
         nil)

       (defmethod closer-mop:validate-superclass ((class ,structure-class-name) (superclass (eql (find-class 'standard-object))))
         ;; Only STRUCTURE-OBJECT may have STANDARD-OBJECT as a direct superclass, all
         ;; other structure classes must inherit from STRUCTURE-OBJECT.
         #-(or abcl clasp) (eql (class-name class) ',structure-object-name)
         #+(or abcl clasp) t)

       (defclass ,structure-object-name (standard-object)
         ()
         (:metaclass ,structure-class-name))

       (defmethod closer-mop:compute-default-initargs ((class ,structure-class-name))
         ;; Modify the default initargs behaviour to stop default initargs from
         ;; being inherited from superclasses.
         ;; This, along with help from defstruct, allows the inheritance behaviour
         ;; for slot initforms to be implemented properly.
         (remove-duplicates
          (closer-mop:class-direct-default-initargs class)
          :key #'first :from-end t))

       (defmethod closer-mop:direct-slot-definition-class ((class ,structure-class-name) &rest initargs)
         (declare (ignore initargs))
         (find-class 'structure-direct-slot-definition))

       (defmethod closer-mop:effective-slot-definition-class ((class ,structure-class-name) &rest initargs)
         (declare (ignore initargs))
         (find-class 'structure-effective-slot-definition))

       (defmethod structure-object-name ((client ,client-class))
         ',structure-object-name)

       (defmethod print-object ((object ,structure-object-name) stream)
         (print-structure object stream))

       (defmethod closer-mop:compute-effective-slot-definition :around ((class ,structure-class-name) name direct-slot-definitions)
         (declare (ignore name))
         (let ((read-only (structure-slot-definition-read-only (first direct-slot-definitions))))
           ;; Validate the read-only slot. The D-S-D list is sorted in precedence order,
           ;; so the value of read-only slot can only move from true to false, not the
           ;; other way around.
           (let ((current-read-only read-only))
             (dolist (slot direct-slot-definitions)
               (cond (current-read-only
                      (setf current-read-only (structure-slot-definition-read-only slot)))
                     (t
                      ;; Can't go from not read-only to read-only.
                      (when (structure-slot-definition-read-only slot)
                        (error 'included-slot-must-be-read-only
                               :slot-name (closer-mop:slot-definition-name slot)))))))
           (let ((effective-slot (call-next-method)))
             (setf (slot-value effective-slot '%read-only) read-only)
             effective-slot)))

       (defmethod (setf closer-mop:slot-value-using-class) :before (new-value (class ,structure-class-name) object (slot structure-effective-slot-definition))
         (declare (ignore new-value))
         (when (and (structure-slot-definition-read-only slot)
                    ;; As a special exception, allow unbound/uninitialized slots to
                    ;; be initialized.
                    (closer-mop:slot-boundp-using-class class object slot))
           (cerror "Set slot anyway" 'slot-is-read-only
                   :object object :slot-name (closer-mop:slot-definition-name slot))))

       (defmethod closer-mop:slot-makunbound-using-class :before ((class ,structure-class-name) object (slot structure-effective-slot-definition))
         (when (and (structure-slot-definition-read-only slot)
                    ;; As a special exception, allow unbound/uninitialized slots to
                    ;; be initialized.
                    (closer-mop:slot-boundp-using-class class object slot))
           (cerror "Make slot unbound anyway" 'slot-is-read-only
                   :object object :slot-name (closer-mop:slot-definition-name slot)))))))
