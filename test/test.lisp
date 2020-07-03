;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package :simple-csp-solver-test)

(defsuite* :simple-csp-solver-test)

(deftest test.1
  (is (equal '(1 2 3) (var-domain (make-var '(1 2 3)))))
  (is (equal '(a b) (var-domain (make-var '(a b)))))
  (is (equal 'foo (var-name (make-var '(a b) 'foo))))
  (is (equal nil (var-name (make-var '(a b))))))

(deftest test.2
  (let ((a (make-var '(1 2 3) 'a))
        (b (make-var '(1 2 3) 'b)))
    (is (null (var-constraints a)))
    (is (null (var-constraints b)))
    (is-true (constraint #'< a b))
    (is (not (null (var-constraints a))))
    (is (not (null (var-constraints b))))))

(deftest test.3
  (let ((a (make-var '(1 2 3) 'a))
        (b (make-var '(1 2 3) 'b)))
    (is (equal '(1 1) (search-one (list a b))))))

(deftest test.4
  (let ((a (make-var '(1 2 3) 'a))
        (b (make-var '(a b) 'b)))
    (is (equal '(1 a) (search-one (list a b))))))

(deftest test.5
  (let ((a (make-var '(1 2 3) 'a))
        (b (make-var '(1 2 3) 'b))
        (c (make-var '(1 2 3) 'c)))
    (constraint #'< a b c)
    (is (equal '(1 2 3) (search-one (list a b c))))))

(deftest test.6
  (let ((a (make-var '(1 2 3) 'a))
        (b (make-var '(a b) 'b)))
    (is (equal '((1 a) (1 b) (2 a) (2 b) (3 a) (3 b))
               (search-all (list a b))))))

(deftest test.7
  (let ((a (make-var '(10 20 30) 'a))
        (b (make-var '(1 2 3) 'b)))
    (constraint #'< a b)
    (is (equal nil (search-all (list a b))))))

(deftest test.8
  (let ((a (make-var '(1 2 3) 'a))
        (b (make-var '(1 2 3) 'b)))
    (constraint #'< a b)
    (constraint #'> a b)
    (is (equal nil (search-all (list a b))))
    (is (equal nil (search-one (list a b))))))

(deftest test.9
  (let ((a (make-var '(1 2 3) 'a))
        (b (make-var '(1 2 3) 'b)))
    (constraint #'<= a b)
    (constraint #'>= a b)
    (is (equal '((1 1) (2 2) (3 3))
               (search-all (list a b))))))

(deftest test.10
  (let ((a (make-var '(1 2 3) 'a))
        (b (make-var '(a b) 'b)))
    (is (equal '((1 a) (1 b) (2 a) (2 b) (3 a) (3 b))
               (search-n (list a b) 30)))
    (is (equal '((1 a) (1 b) (2 a))
               (search-n (list a b) 3)))
    (is (equal '((1 a))
               (search-n (list a b) 1)))
    (is (equal nil
               (search-n (list a b) 0)))
    (signals error (search-n (list a b) -1))))

(deftest test.11
  (let ((a (make-var '(1 2 3) 'a))
        (b (make-var '(a b) 'b)))
    (is (equal 6 (count-solutions (list a b))))
    (is (equal 3 (count-solutions (list a))))
    (is (equal 0 (count-solutions (list))))))

(deftest test.12
  (let* ((d (iota 15 :start 1))
         (a (make-var d 'a))
         (b (make-var d 'b))
         (c (make-var d 'c)))
    (constraint (lambda (a b c) (= (+ (* a a) (* b b)) (* c c)))
                a b c)
    (constraint #'< a b)
    (constraint #'< a c)
    (constraint #'< b c)
    (is (equal '((3 4 5) (5 12 13) (6 8 10) (9 12 15))
               (search-all (list a b c))))))

(deftest test.13
  (is (equal
       '(0 0 0 0 1 1 1 1 1 2 2 2 3 3 4 4 5 5 5 6 6 6 6 6 8 9 9 9 10 11)
       (mapcar
        (lambda (n)
          (let* ((d (iota n :start 1))
                 (a (make-var d 'a))
                 (b (make-var d 'b))
                 (c (make-var d 'c)))
            (constraint (lambda (a b c) (= (+ (* a a) (* b b)) (* c c)))
                        a b c)
            (constraint #'< a b)
            (constraint #'< a c)
            (constraint #'< b c)
            (count-solutions (list a b c))))
        (iota 30 :start 1)))))

(deftest test.14
  (is (equal
       '(0 0 0 0 1 0 0 0 0 1 0 0 1 0 1 0 1 0 0 1 0 0 0 0 2 1 0 0 1 1)
       (mapcar
        (lambda (n)
          (let* ((d (iota n :start 1))
                 (a (make-var d 'a))
                 (b (make-var d 'b))
                 (c (make-var d 'c)))
            (constraint (lambda (a b c) (= (+ (* a a) (* b b)) (* c c)))
                        a b c)
            (constraint #'< a b)
            (constraint #'< a c)
            (constraint #'< b c)
            (constraint (lambda (x) (= x n)) c)
            (count-solutions (list a b c))))
        (iota 30 :start 1)))))

(deftest test.15
  (labels ((diff (x y d)
             (= d (abs (- x y))))
           (pairwise-distinct (vars)
             (loop for tail on vars
                   for x = (car tail)
                   do (dolist (y (cdr tail))
                        (constraint #'/= x y))))
           (build-vars (n)
             (let* ((dom (iota n :start 1))
                    (vars (loop repeat n collect (make-var dom)))
                    (ivs (loop repeat (1- n) collect (make-var dom))))
               (pairwise-distinct vars)
               (pairwise-distinct ivs)
               ;; vars and ivs
               (loop for tail on vars
                     for iv in ivs
                     for x = (car tail)
                     for y = (second tail)
                     do (constraint #'diff x y iv))
               (append vars ivs)))
           (all-rows (n)
             (search-all (build-vars n)))
           (count-rows (n)
             (count-solutions (build-vars n))))
    (is (equal '((1 2 1) (2 1 1))
               (all-rows 2)))
    (is (equal '((1 3 2 2 1) (2 1 3 1 2) (2 3 1 1 2) (3 1 2 2 1))
               (all-rows 3)))
    (is (equal '((1 4 2 3 3 2 1) (2 3 1 4 1 2 3) (3 2 4 1 1 2 3) (4 1 3 2 3 2 1))
               (all-rows 4)))
    (is (equal '((1 5 2 4 3 4 3 2 1) (2 3 5 1 4 1 2 4 3) (2 5 1 3 4 3 4 2 1)
                 (3 2 4 1 5 1 2 3 4) (3 4 2 5 1 1 2 3 4) (4 1 5 3 2 3 4 2 1)
                 (4 3 1 5 2 1 2 4 3) (5 1 4 2 3 4 3 2 1))
               (all-rows 5)))
    (is (equal 24 (count-rows 6)))
    (is (equal 32 (count-rows 7)))
    (is (equal 40 (count-rows 8)))))

(deftest test.16
  (let* ((d (iota 5 :start 1))
         (a (make-var d 'a))
         (b (make-var d 'b))
         (c (make-var d 'c))
         (d (make-var d 'd)))
    (constraint (lambda (a b c d) (= 15 (+ a b c d))) a b c d)
    (constraint (lambda (a b c) (= 10 (+ a b c))) a b c)
    (constraint (lambda (a b) (= 5 (+ a b))) a b)
    (constraint (lambda (b d) (>= (- d b) 2)) b d)
    (constraint #'< a b)
    (is (equal '((2 3 5 5)) (search-all (list a b c d))))
    (is (equal '((3 2 5 5)) (search-all (list b a c d))))
    (is (equal '((5 2 3 5)) (search-all (list c a b d))))
    (is (equal '((2 5 3 5)) (search-all (list a c b d))))
    (is (equal '((3 5 2 5)) (search-all (list b c a d))))
    (is (equal '((5 3 2 5)) (search-all (list c b a d))))
    (is (equal '((5 3 5 2)) (search-all (list d b c a))))
    (is (equal '((3 5 5 2)) (search-all (list b d c a))))
    (is (equal '((5 5 3 2)) (search-all (list c d b a))))
    (is (equal '((5 5 3 2)) (search-all (list d c b a))))
    (is (equal '((3 5 5 2)) (search-all (list b c d a))))
    (is (equal '((5 3 5 2)) (search-all (list c b d a))))
    (is (equal '((5 2 5 3)) (search-all (list d a c b))))
    (is (equal '((2 5 5 3)) (search-all (list a d c b))))
    (is (equal '((5 5 2 3)) (search-all (list c d a b))))
    (is (equal '((5 5 2 3)) (search-all (list d c a b))))
    (is (equal '((2 5 5 3)) (search-all (list a c d b))))
    (is (equal '((5 2 5 3)) (search-all (list c a d b))))
    (is (equal '((5 2 3 5)) (search-all (list d a b c))))
    (is (equal '((2 5 3 5)) (search-all (list a d b c))))
    (is (equal '((3 5 2 5)) (search-all (list b d a c))))
    (is (equal '((5 3 2 5)) (search-all (list d b a c))))
    (is (equal '((2 3 5 5)) (search-all (list a b d c))))
    (is (equal '((3 2 5 5)) (search-all (list b a d c))))))

(deftest test.17
  (let ((a (make-var '(1 2 3)))
        (b (make-var '(1 2 3))))
    (constraint (curry #'= 2) a)
    (constraint (curry #'= 3) b)
    (is (equal '((2 3))
               (search-all (list a b))))))

(deftest test.18
  (labels ((build-vars (n)
             (let ((vars (loop for i from 1 to n collect (make-var (list 0 i)))))
               (apply #'constraint #'= vars)
               vars)))
    (loop for n from 2 to 16
          do (is (equal (list (make-list n :initial-element 0))
                        (search-all (build-vars n)))))))

(deftest pairwise-distinct.50
  (labels ((pairwise-distinct (vars)
             (loop for tail on vars
                for x = (car tail)
                do (dolist (y (cdr tail))
                     (constraint #'/= x y)))))
    (let* ((n 50)
           (vars (loop repeat n collect (make-var (iota n)))))
      (pairwise-distinct vars)
      (is (equal (iota n) (search-one vars))))))

(deftest pairwise-distinct.500
  (labels ((pairwise-distinct (vars)
             (loop for tail on vars
                for x = (car tail)
                do (dolist (y (cdr tail))
                     (constraint #'/= x y)))))
    (let* ((n 500)
           (vars (loop repeat n collect (make-var (iota n)))))
      (pairwise-distinct vars)
      (is (equal (iota n) (search-one vars))))))
