#lang racket

(define (cube x)
    (* x x x))

(define (sqr x)
    (* x x))

(define (good-enough? guess x)
    (< (abs (- (cube guess) x)) 0.0001))

(define (improve guess x)
    (/ (+ (/ x (sqr guess)) 
          (* 2 guess)) 
       3))

(define (cube-sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (cube-sqrt-iter (improve guess x) x)))

(define (cube-sqrt x)
    (cube-sqrt-iter 1.0 x))

(display (cube-sqrt 8))
