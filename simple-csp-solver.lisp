;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package :simple-csp-solver)

(defstruct (var (:constructor %make-var))
  domain name constraints)

(defun make-var (domain &optional name)
  (%make-var :domain domain :name name))

(defmethod print-object ((self var) stream)
  (pprint-logical-block (stream nil)
    (print-unreadable-object (self stream :type t)
      (format stream "~_DOMAIN: ~S ~_NAME: ~S ~_CONSTRAINTS: ~S"
              (var-domain self)
              (var-name self)
              (length (var-constraints self))))))

(defclass constraint ()
  ((predicate :accessor constraint-predicate :initarg :predicate)
   (vars :accessor constraint-vars :initarg :vars)))

(defmethod print-object ((self constraint) stream)
  (pprint-logical-block (stream nil)
    (print-unreadable-object (self stream :type t)
      (format stream "~_PREDICATE: ~S ~_VARS: ~S"
              (constraint-predicate self)
              (constraint-vars self)))))

(defun constraint (predicate var &rest vars)
  (let ((vars (cons var vars)))
    (let ((constraint (make-instance 'constraint
                                     :predicate predicate
                                     :vars vars)))
      (dolist (var vars constraint)
        (push constraint (var-constraints var))))))

(defclass global-constraint (constraint)
  ())

(defun global-constraint (predicate vars)
  (let ((constraint (make-instance 'global-constraint
                                   :predicate predicate
                                   :vars vars)))
    (dolist (var vars constraint)
      (push constraint (var-constraints var)))))

(macrolet ((frob (n)
             (let ((symbols (loop repeat n collect (gensym))))
               `(defun ,(symbolicate "FUNCALL-WITH-AREF-" (princ-to-string n))
                    (fn indices)
                  (declare (function fn) (cons indices))
                  (destructuring-bind ,symbols indices
                    (declare (array-index ,@symbols))
                    (lambda (vars)
                      (declare (optimize speed (safety 0) (debug 0)))
                      (declare (simple-vector vars))
                      (funcall fn ,@(mapcar (lambda (s) `(aref vars ,s)) symbols)))))))
           (quux (n)
             `(progn ,@(mapcar (lambda (x) `(frob ,x))
                               (iota n :start 1)))))
  (quux 12))

(defun apply-with-aref (fn indices)
  (declare (function fn) (cons indices))
  (let ((values (make-list (length indices))))
    (lambda (vars)
      #+nil(declare (optimize speed (safety 0) (debug 0)))
      (declare (simple-vector vars))
      (apply fn (map-into values (lambda (i) (aref vars i)) indices)))))

(defun wrap-predicate (predicate indices)
  (declare (function predicate) (cons indices))
  (case (length indices)
    (1 (funcall-with-aref-1 predicate indices))
    (2 (funcall-with-aref-2 predicate indices))
    (3 (funcall-with-aref-3 predicate indices))
    (4 (funcall-with-aref-4 predicate indices))
    (5 (funcall-with-aref-5 predicate indices))
    (6 (funcall-with-aref-6 predicate indices))
    (7 (funcall-with-aref-7 predicate indices))
    (8 (funcall-with-aref-8 predicate indices))
    (9 (funcall-with-aref-9 predicate indices))
    (10 (funcall-with-aref-10 predicate indices))
    (11 (funcall-with-aref-11 predicate indices))
    (12 (funcall-with-aref-12 predicate indices))
    (t (apply-with-aref predicate indices))))

(defmethod build-constraint-predicate ((constraint constraint) indices)
  (wrap-predicate (constraint-predicate constraint) indices))

(defmethod build-constraint-predicate ((constraint global-constraint) indices)
  (let ((prefix-indices (butlast indices))
        (last-index (car (last indices)))
        (prefix-list (make-list (1- (length indices)))))
    (lambda (vars)
      (funcall (constraint-predicate constraint)
               (map-into prefix-list (lambda (i) (aref vars i)) prefix-indices)
               (aref vars last-index)))))

(defun find-max (list)
  (declare (cons list))
  (loop for x in list maximize x))

(defun map-vector-internal (fn domains state vars n constraint-vector)
  (declare (optimize speed (safety 0) (debug 0)))
  (declare (simple-vector domains state vars constraint-vector))
  (declare (function fn))
  (declare (fixnum n))
  (let ((pos 0))
    (declare (fixnum pos))
    (macrolet ((update-vars ()
                 '(setf (aref vars pos) (pop (aref state pos))))
               (backtrack-needed ()
                 '(null (aref state pos)))
               (backtrack ()
                 '(progn
                   (setf (aref state pos) (aref domains pos))
                   (decf pos)
                   (when (= -1 pos)
                     (return))))
               (partial-solution-ok ()
                 '(block nil
                   (dolist (constraint (aref constraint-vector pos) t)
                     (unless (funcall (the function constraint) vars)
                       (return nil)))))
               (forward ()
                 '(incf pos))
               (at-last-pos ()
                 '(= pos (1- n))))
      (loop
         (if (backtrack-needed)
             (backtrack)
             (progn
               (update-vars)
               (when (partial-solution-ok)
                 (if (at-last-pos)
                     (funcall fn vars)
                     (forward)))))))))

(defun map-vector (fn domains constraint-vector)
  (let ((domains (coerce domains 'simple-vector))
        (state (coerce domains 'simple-vector))
        (vars (coerce domains 'simple-vector))
        (n (length domains)))
    (map-vector-internal fn domains state vars n constraint-vector)))

(defun make-var-indices (vars)
  (let ((hash (make-hash-table)))
    (loop
       for var in vars
       for i upfrom 0
       do (setf (gethash var hash) i))
    hash))

(defun constraint-indices (constraint var-indices)
  (mapcar (lambda (var) (gethash var var-indices))
          (constraint-vars constraint)))

(defmethod add-constraint-to-constraint-vector ((constraint constraint) constraint-vector var-indices)
  (let* ((indices (constraint-indices constraint var-indices))
         (max-index (find-max indices)))
    (push
     (build-constraint-predicate constraint indices)
     (aref constraint-vector max-index))))

(defmethod add-constraint-to-constraint-vector ((constraint global-constraint) constraint-vector var-indices)
  (let ((indices (sort (constraint-indices constraint var-indices) #'<)))
    (loop
       for i from 2 to (length indices)
       for sub-indices = (subseq indices 0 i)
       for max-index = (nth (1- i) indices)
       do (push
           (build-constraint-predicate constraint sub-indices)
           (aref constraint-vector max-index)))))

(defun build-constraint-vector (vars constraints)
  (let ((constraint-vector (make-array (length vars) :initial-element nil))
        (var-indices (make-var-indices vars)))
    (dolist (constraint constraints constraint-vector)
      (add-constraint-to-constraint-vector constraint constraint-vector var-indices))))

(defun collect-constraints (vars)
  (let ((hash (make-hash-table)))
    (dolist (var vars)
      (dolist (constraint (var-constraints var))
        (setf (gethash constraint hash) t)))
    (hash-table-keys hash)))

(defun map-solutions (fn vars)
  (when vars
    (map-vector
     fn
     (mapcar #'var-domain vars)
     (build-constraint-vector vars (collect-constraints vars)))))

(defun search-one (vars)
  (map-solutions
   (lambda (solution)
     (declare (simple-vector solution))
     (return-from search-one (coerce solution 'list)))
   vars))

(defun search-all (vars)
  (let (solutions)
    (map-solutions
     (lambda (solution)
       (declare (simple-vector solution))
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
           (declare (simple-vector solution))
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
