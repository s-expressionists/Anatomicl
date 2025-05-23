;;;; Expand structure-object-based defstructs.

(cl:in-package #:anatomicl)

(defun check-included-structure-object (client description environment)
  (when (defstruct-included-structure-name description)
    (let* ((parent-name (defstruct-included-structure-name description))
           (included-structure (find-class client parent-name nil environment))
           (expected-type (structure-class-name client)))
      (unless included-structure
        (if (structure-description client parent-name environment)
            (error 'included-structure-must-not-be-typed :name parent-name)
            (error 'included-structure-does-not-exist :name parent-name)))
      (unless (typep included-structure expected-type)
        (error 'included-structure-must-be-structure
               :name parent-name :datum included-structure
               :expected-type expected-type))
      (unless (closer-mop:class-finalized-p included-structure)
        (closer-mop:finalize-inheritance included-structure))
      ;; All included slots must be present in the included structure.
      (dolist (slot (defstruct-included-slots description))
        (let ((existing (find (slot-name slot)
                              (closer-mop:class-slots included-structure)
                              :key #'closer-mop:slot-definition-name
                              :test #'string=)))
          (unless existing
            (error 'included-slot-missing-from-parent
                   :slot-name (slot-name slot)))
          ;; For the sake of sanity, lets require them to be the same symbol too.
          ;; If it is legal for them to be different, then they need to be
          ;; canonicalized to the existing slot so that slot inheritance works
          ;; correctly.
          (unless (eql (slot-name slot) (closer-mop:slot-definition-name existing))
            (error 'included-slot-conflicts-with-parent-slot
                   :slot-name (slot-name slot)
                   :parent-slot-name (closer-mop:slot-definition-name existing)))))
      ;; Direct slots must not be present (string=)
      (dolist (slot (defstruct-direct-slots description))
        (let ((existing (find (slot-name slot)
                              (closer-mop:class-slots included-structure)
                              :key #'closer-mop:slot-definition-name
                              :test #'string=)))
          (when existing
            (error 'direct-slot-conflicts-with-parent-slot
                   :slot-name (slot-name slot)
                   :parent-slot-name (closer-mop:slot-definition-name existing))))))))

(defmethod compute-slot-layout (client (description defstruct-object-description) environment)
  (check-included-structure-object client description environment)
  ;; All of them, including implicitly included slots.
  (append
   (when (defstruct-included-structure-name description)
     (loop with included-structure = (find-class client
                                                 (defstruct-included-structure-name description)
                                                 t
                                                 environment)
           with included-slots = (defstruct-included-slots description)
           with default-initargs = (closer-mop:class-direct-default-initargs included-structure)
           for slot in (closer-mop:class-slots included-structure)
           for slot-name = (closer-mop:slot-definition-name slot)
           for inclusion = (find slot-name included-slots :key #'slot-name)
           collect (or inclusion
                       ;; For implicitly included slots, reconstruct a slot-description
                       ;; from the effective slot-definition.
                       (let* ((initarg-kw (keywordify slot-name))
                              (default-initarg (find initarg-kw default-initargs :key #'first)))
                         (make-instance 'slot-description
                                        :name slot-name
                                        ;; Generate an accessor with the right name,
                                        ;; don't use the one generated for the parent.
                                        :accessor-name (compute-accessor-name description slot-name)
                                        :initform (second default-initarg)
                                        :initform-p (not (not default-initarg))
                                        :type (closer-mop:slot-definition-type slot)
                                        :read-only (structure-slot-definition-read-only slot))))))
   (defstruct-direct-slots description)))

(defmethod layout-slots (client (description defstruct-object-description) layout)
  (declare (ignore client))
  layout)

(defmethod generate-allocation-form (client (description defstruct-object-description) all-slots)
  (declare (ignore client all-slots))
  `(allocate-instance (cl:find-class ',(defstruct-name description))))

(defmethod generate-slot-initialization-form (client (description defstruct-object-description) layout object slot value)
  (declare (ignore layout))
  (declare (ignore client))
  `(setf (slot-value ,object ',(slot-name slot)) ,value))

(defmethod generate-predicate (client (description defstruct-object-description) layout predicate-name)
  (declare (ignore client layout))
  `(defun ,predicate-name (object)
     (typep object ',(defstruct-name description))))

(defmethod generate-copier (client (description defstruct-object-description) layout copier-name)
  (declare (ignore layout))
  `(defun ,copier-name (object)
     (check-type object ,(defstruct-name description))
     (copy-structure ,(client-form client) object)))

(defun compute-structure-object-direct-slots (all-slots)
  (loop for slot in all-slots
        collect `(list :name ',(slot-name slot)
                       :type ',(slot-type slot)
                       :read-only ',(slot-read-only slot)
                       :initargs '(,(keywordify (slot-name slot)))
                       :readers '(,(slot-accessor-name slot))
                       ,@(unless (slot-read-only slot)
                           `(:writers '((setf ,(slot-accessor-name slot))))))))

(defun compute-structure-object-direct-default-initargs (all-slots)
  (loop for slot in all-slots
        when (slot-initform-p slot)
          collect `(list ',(keywordify (slot-name slot))
                         ',(slot-initform slot)
                         (lambda () ,(slot-initform slot)))))

(defmethod generate-defstruct-bits (client (description defstruct-object-description) layout environment)
  (declare (ignore environment))
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (closer-mop:ensure-class
        ',(defstruct-name description)
        :metaclass ',(structure-class-name client)
        :direct-superclasses '(,(or (defstruct-included-structure-name description)
                                    (structure-object-name client)))
        :direct-slots (list ,@(compute-structure-object-direct-slots layout))
        :direct-default-initargs (list ,@(compute-structure-object-direct-default-initargs layout))
        :standard-constructor ',(some (lambda (ctor) (endp (rest ctor)))
                                        (defstruct-constructors description))))
     ,@(when (defstruct-print-object description)
         (list `(defmethod print-object ((object ,(defstruct-name description)) stream)
                  (funcall (function ,(defstruct-print-object description)) object stream))))))
