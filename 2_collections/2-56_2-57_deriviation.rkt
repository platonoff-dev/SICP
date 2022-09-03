#lang racket

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a b) 
  (cond [(=number? a 0) b]
        [(=number? b 0) a]
        [(and (number? a) (number? b)) (+ a b)]
        [else (list '+ a b)]))

(define (make-product a b)
  (cond [(or (=number? a 0) (=number? b 0)) 0]
        [(=number? a 1) b]
        [(=number? b 1) a]
        [(and (number? a) (number? b)) (* a b)]
        [else (list '* a b)]))

(define (make-exponentiation u n)
  (cond [(=number? u 1) 1]
        [(=number? u 0) 0]
        [(=number? n 1) u]
        [(=number? n 0) 1]
        [(and (number? u) (number? n)) (expt u n)]
        [else (list '** u n)]))

(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend x) (cadr x))
(define (augend x) (if (null? (cdddr x)) (caddr x) (cons '+ (cddr x))))

(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier x) (cadr x))
(define (multiplicand x) (if (null? (cdddr x)) (caddr x) (cons '* (cddr x))))

(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))
(define (base x) (cadr x))
(define (exponent x) (caddr x))

(define (deriv exp var) 
  (cond [(number? exp) 0]
        [(variable? exp) (if (same-variable? exp var) 1 0)] 
        [(sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var))]
        [(product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp)))]
        [(exponentiation? exp)
          (make-product (exponent exp)
                        (make-exponentiation (base exp) (make-sum (exponent exp) -1)))]
        [else (error "unknown expression type: DERIV" exp)]))


(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* x y (+ x 3)) 'x)
(deriv '(** x 3) 'x)
(deriv '(+ x (* 2 x) y (* 2 x)) 'x)