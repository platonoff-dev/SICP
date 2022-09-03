#lang racket


(define (equal? a b)
  (cond [(and (null? a) (null? b)) true]
        [(and (symbol? a) (symbol? b) (eq? a b)) true]
        [(and (pair? a) (pair? b) (equal? (car a) (car b)) (equal? (cdr a) (cdr b))) true]
        [else false]))

(equal? '(this is a list) '(this is a list))
(equal? '(this is a list) '(this (is a) list))
