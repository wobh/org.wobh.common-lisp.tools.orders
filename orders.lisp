;; -*- mode: lisp -*-

(defpackage #:org.wobh.common-lisp.tools.orders
  (:use #:common-lisp)
  (:nicknames #:orders)
  (:export #:make-index-map
           #:make-index-cycles
           #:make-permuter
	   #:compose-index-cycles
	   #:cycle-permuter
	   #:map-permuters)
  (:documentation "ORG.WOBH.COMMON-LISP.TOOLS.ORDERS

Provides ordering utilities for permutations and combinations."))

(in-package #:org.wobh.common-lisp.tools.orders)

;;; Permutations

(defun make-index-map (sequence &key
                                  (same-test #'eql)
                                  (sort-test #'<)
                                  (sort-key #'identity))
  (let ((index-seq (sort (copy-seq sequence)
                         sort-test
                         :key sort-key)))
    (flet ((lookup (item)
             (position item
                       index-seq
                       :test same-test)))
      (map 'list #'lookup sequence))))

(defun make-index-cycles (sequence
                          &key
                            (same-test #'eql)
                            (sort-test #'<)
                            (sort-key #'identity))
  (loop
    with index-map = (make-index-map sequence
                                     :same-test same-test
                                     :sort-test sort-test
                                     :sort-key  sort-key)
    with indexes = (loop
                     for item below (length sequence)
                     collect item)
    for outer = (first indexes)
    while indexes
    collect (loop
              for inner = outer then (elt index-map inner)
              collect inner into cycle
              until (and (= inner (first cycle))
                         (< 1 (length cycle)))
              finally (return
                        (progn
                          (setf indexes (sort (nset-difference indexes cycle) #'<))
                          (when (< 2 (length cycle))
                            (butlast cycle)))))
      into cycles
    finally (return
              (remove-if #'null cycles))))

(defun make-permuter (permutation)
  (flet ((permuter (sequence)
           (loop
              with out = (copy-seq sequence)
              with len = (length sequence)
              for cycle in permutation
              if (< 1 (length cycle))
              do (loop
                    for (here there) on cycle by #'cdr
                    if (and there
                            (> len here)
                            (> len there))
                      do (rotatef (elt out here) (elt out there)))
              finally (return out))))
    (setf (documentation #'permuter 'function)
          (format nil "Permutes sequence with `~A'." permutation))
    #'permuter))

(defun compose-index-cycles (index-cycle-1 index-cycle-2)
  (make-index-cycles
   (funcall (make-permuter index-cycle-2)
            (funcall (make-permuter index-cycle-1)
                     (loop
                       for index to (reduce #'max
                                            (reduce #'append
                                                    (append index-cycle-1
                                                            index-cycle-2)))
                       collect index)))))

(defun cycle-permuter (permuter sequence &key (same-test #'equal))
  (loop
    for this = sequence then (funcall permuter this)
    collect this into permutations
    until (and (funcall same-test this (first permutations))
               (< 1 (length permutations)))
    finally (return (butlast permutations))))

;; CAUTION: have to use `EQUALP' for `same-test' when the sequence is
;; a vector.

(defun map-permuters (sequence &rest permuters)
  (loop
    for permuter in permuters
    for this = (funcall permuter sequence) then (funcall permuter this)
    collect this))


;;; Combinations

;; TODO
