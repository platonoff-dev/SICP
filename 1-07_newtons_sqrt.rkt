#lang racket

(define (average x y)
    (/ (+ x y) 2))

(define (sqr x)
    (* x x))

(define (good-enough? guess x)
    (< (abs (- (sqr guess) x)) 0.0001))

(define (improve guess x)
    (average guess (/ x guess)))

(define (sqrt-newton guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-newton (improve guess x) x)))

(define (sqrt x)
    (sqrt-newton 1.0 x))

(display (sqrt 4))
