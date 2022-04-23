;; -*- mode: lisp -*-

(defpackage #:org.wobh.common-lisp.tools.orders-test
  (:use #:common-lisp)
  (:local-nicknames (#:orders #:org.wobh.common-lisp.tools.orders))
  (:documentation "ORG.WOBH.COMMON-LISP.TOOLS.ORDERS-TEST

Basic tests for ORDERS. If this file loads without errors, the
tests passed."))

(in-package #:org.wobh.common-lisp.tools.orders-test)

(let ((subject '(#\b #\e #\d #\c #\a))
      (expect '((0 1 4) (2 3))))
  (assert (equal expect
                 (orders:make-index-cycles subject
                                           :sort-test #'char<))))

(let ((subject '(#\a #\b #\c)))
  (let ((expect '(#\a #\b #\c)))
    (let ((object '((0) (1) (2))))
      (assert (equal expect
                     (funcall (orders:make-permuter object)
                              subject))))
    (let ((object nil))
      (assert (equal expect
                     (funcall (orders:make-permuter object)
                              subject)))))
  (let ((expect '(#\b #\a #\c)))
    (let ((object '((0 1) (2))))
      (assert (equal expect
                     (funcall (orders:make-permuter object)
                              subject))))
    (let ((object '((0 1))))
      (assert (equal expect
                     (funcall (orders:make-permuter object)
                              subject)))))
  (let ((expect '(#\c #\b #\a)))
    (let ((object '((0 2) (1))))
      (assert (equal expect
                     (funcall (orders:make-permuter object)
                              subject))))
    (let ((object '((0 2))))
      (assert (equal expect
                     (funcall (orders:make-permuter object)
			      subject)))))
  (let ((expect '(#\a #\c #\b)))
    (let ((object '((0) (1 2))))
      (assert (equal expect
                     (funcall (orders:make-permuter object)
                              subject))))
    (let ((object '((1 2))))
      (assert (equal expect
                     (funcall (orders:make-permuter object)
                              subject)))))
  (let ((expect '(#\c #\a #\b)))
    (let ((object '((0 2 1))))
      (assert (equal expect
                     (funcall (orders:make-permuter object)
                              subject))))
    (let ((object '((1 0 2))))
      (assert (equal expect
                     (funcall (orders:make-permuter object)
                              subject)))))
  (let ((expect '(#\b #\c #\a)))
    (let ((object '((0 1 2))))
      (assert (equal expect
                     (funcall (orders:make-permuter object)
                              subject))))
    (let ((object '((2 0 1))))
      (assert (equal expect
                     (funcall (orders:make-permuter object)
                              subject))))))

(let ((subject1 '((0 4 1) (2 3)))
      (subject2 '((2 6) (5 7)))
      (expect '((0 4 1) (2 6 3) (5 7))))
  (assert (equal expect
                 (orders:compose-index-cycles subject1
                                              subject2))))

(let* ((expect '(3 5 2 4 7 8 1 6))
       (subject (sort (copy-seq expect) #'<)))
  (assert (equal expect
                 (funcall
                  (orders:make-permuter (orders:make-index-cycles expect))
                  subject))))

(let ((subject (orders:make-permuter '((0 4 1) (2 3))))
      (object '(#\a #\b #\c #\d #\e))
      (expect '((#\a #\b #\c #\d #\e)
                (#\e #\a #\d #\c #\b)
                (#\b #\e #\c #\d #\a)
                (#\a #\b #\d #\c #\e)
                (#\e #\a #\c #\d #\b)
                (#\b #\e #\d #\c #\a))))
  (assert (equal expect
                 (orders:cycle-permuter subject
                                        object))))

(let ((expect '((#\a #\c #\b)
                (#\b #\a #\c)
                (#\b #\c #\a)
                (#\c #\a #\b)
                (#\c #\b #\a))))
  (assert (equal expect
                 (orders:map-permuters '(#\a #\b #\c)
                                       (orders:make-permuter '((1 2)))
                                       (orders:make-permuter '((0 2 1)))
                                       (orders:make-permuter '((1 2)))
                                       (orders:make-permuter '((0 1 2)))
                                       (orders:make-permuter '((1 2)))))))
