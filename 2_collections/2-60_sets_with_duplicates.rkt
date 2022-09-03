#lang racket


(define (adjoin-set x set)
  (cons x set))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (cond 
    [(or (null? set1) (null? set2)) '()]
    [(element-of-set? (car set1) set2) (cons (car set1) (intersection-set (cdr set1) set2))]
    [else (intersection-set (cdr set1) set2)]))

(define (union-set set1 set2)
  (append set1 set2))

(define s1 (adjoin-set 0 (list 1 3 5 7)))
(define s2 (adjoin-set 0 (list 2 4 6 8)))

(union-set s1 s2)
(intersection-set s1 s2)
(adjoin-set 123000 s1)
(element-of-set? 123000 s1)
(element-of-set? 0 s1)