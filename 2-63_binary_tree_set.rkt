#lang sicp

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree enrty left right)
  (list entry left right))

(define (element-of-set? x s)
  (cond ((null? s) false)
        ((= x (entry s)) true)
        ((< x (entry s))
          (element-of-set? x (left-branch s)))
        ((> x (entry s))
          (element-of-set? x (right-branch s)))))
