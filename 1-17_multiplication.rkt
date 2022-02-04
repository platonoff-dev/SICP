#lang racket

(require racket/trace)

(define (halve x) (/ x 2))

(define (double x) (* x 2))

(define (fast-multiply a b)
    (cond 
        [(or (= a 0) (= b 0)) 0]
        [(= b 1) a]
        [(even? b) (double (fast-multiply a (halve b)))]
        [else (+ a (fast-multiply a (- b 1)))]
    )
)


(define (i-fast-multiply a b)
    (define (fast-multiply-iter a b res)
        (cond 
            [(= b 0) res]
            [(even? b) (fast-multiply-iter (double a) (halve b) res)]
            [else (fast-multiply-iter a (- b 1) (+ res a))]
        )
    )

    (if (or (= a 0) (= b 0)) 0 (fast-multiply-iter a b 0))
)

(i-fast-multiply 8 8)
