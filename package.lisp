;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(defpackage :simple-csp-solver
  (:use :common-lisp :alexandria)
  (:nicknames :scs)
  (:export
   #:make-var
   #:var-domain
   #:var-name
   #:var-constraints
   #:constraint
   #:search-one
   #:search-all
   #:search-n
   #:count-solutions))
