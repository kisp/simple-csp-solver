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
