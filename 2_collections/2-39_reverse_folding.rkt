#lang racket


(define (fold-right f initial collection)
  (define (iter result collection)
    (if (empty? collection)
      result
      (iter (f result (first collection)) (rest collection))))
  (iter initial collection))


(define (fold-left f initial collection)
  (if (empty? collection)
    initial
    (f (first collection) (fold-left f initial (rest collection)))))


(println (fold-right list null '(1 2 3)))
(println (fold-left list null '(1 2 3)))


(define (reverse-fold-right sequence)
    (fold-right 
        (lambda (x y) '()) null sequence))


(define (reverse-fold-left sequence)
    (fold-left 
        (lambda (x y) '()) null sequence))

