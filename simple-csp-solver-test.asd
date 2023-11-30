;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package :asdf-user)

(defsystem :simple-csp-solver-test
  :name "simple-csp-solver-test"
  :description "Tests for simple-csp-solver"
  :components ((:module "test"
                :components ((:file "package")
                             (:file "test" :depends-on ("package")))))
  :depends-on (:simple-csp-solver :fiveam :alexandria))

(defmethod perform ((op test-op)
                    (system (eql (find-system :simple-csp-solver-test))))
  (perform op (find-system :simple-csp-solver)))
