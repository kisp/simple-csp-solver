;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package :simple-csp-solver)

(defstruct (var (:constructor %make-var))
  domain name constraints)

(defun make-var (domain &optional name)
  (%make-var :domain domain :name name))

(defstruct (constraint (:constructor %make-constraint))
  predicate vars)

(defun constraint (predicate var &rest vars)
  (let ((vars (cons var vars)))
    (let ((constraint (%make-constraint :predicate predicate
                                        :vars vars)))
      (dolist (var vars constraint)
        (push constraint (var-constraints var))))))

(defun map-solutions (fn vars)
  (let ((constraints (when vars (reduce #'union vars :key #'var-constraints))))
    (labels ((check (solution constraint)
               (apply (constraint-predicate constraint)
                      (mapcar (lambda (var)
                                (nth (position var vars) solution))
                              (constraint-vars constraint)))))
      (when vars
        (remove
         nil
         (apply #'map-product
                (lambda (&rest args)
                  (when (every (curry #'check args) constraints)
                    (funcall fn args)))
                (mapcar #'var-domain vars)))))))

(defun search-one (vars)
  (map-solutions
   (lambda (solution)
     (return-from search-one (coerce solution 'list)))
   vars))

(defun search-all (vars)
  (let (solutions)
    (map-solutions
     (lambda (solution)
       (push (coerce solution 'list) solutions))
     vars)
    (nreverse solutions)))

(defun search-n (vars n)
  (check-type n (integer 0))
  (unless (zerop n)
    (let (solutions
          (count 0))
      (block nil
        (map-solutions
         (lambda (solution)
           (push (coerce solution 'list) solutions)
           (incf count)
           (when (= count n)
             (return)))
         vars))
      (nreverse solutions))))

(defun count-solutions (vars)
  (let ((count 0))
    (map-solutions
     (lambda (solution)
       (declare (ignore solution))
       (incf count))
     vars)
    count))
