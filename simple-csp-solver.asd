;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package :asdf-user)

(defsystem :simple-csp-solver
  :name "simple-csp-solver"
  :description "A simple CSP solver that uses backtracking on arbitrary lisp predicates."
  :author "Kilian Sprotte <kilian.sprotte@gmail.com>"
  :version #.(with-open-file
                 (vers (merge-pathnames "version.lisp-expr" *load-truename*))
               (read vers))
  :components ((:static-file "version" :pathname #p"version.lisp-expr")
               (:file "package")
               (:file "simple-csp-solver" :depends-on ("package"))
               )
  :depends-on (:alexandria))

(defmethod perform ((op test-op)
                    (system (eql (find-system :simple-csp-solver))))
  (oos 'load-op :simple-csp-solver-test)
  (funcall (intern "RUN!" "MYAM") :simple-csp-solver-test))
