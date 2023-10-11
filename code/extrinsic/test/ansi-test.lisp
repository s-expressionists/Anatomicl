(in-package #:anatomicl-extrinsic/test)

(defun test ()
  (let ((system (asdf:find-system :anatomicl-extrinsic/test)))
    (ansi-test-harness:ansi-test :directory (merge-pathnames (make-pathname :directory '(:relative "dependencies" "ansi-test"))
                                                             (asdf:component-pathname system))
                                 :expected-failures (asdf:component-pathname (asdf:find-component system
                                                                                                  '("code" "expected-failures.sexp")))
                                 :extrinsic-symbols '(anatomicl-extrinsic:copy-structure
                                                      anatomicl-extrinsic:defstruct
                                                      anatomicl-extrinsic:structure-class
                                                      anatomicl-extrinsic:structure-object)
                                 :tests '("STRUCT")
                                 :exit t)))
