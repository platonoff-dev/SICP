#lang racket
(require racket/trace)

(define (expt b n)
    (define (expt-iter a b n)
        (cond ((= n 0) a)
            ((even? n) (expt-iter a (* b b) (/ n 2)))
            (else (expt-iter (* a b) b (- n 1)))
        )
    )

    (trace expt-iter)
    (expt-iter 1 b n)
)

(expt 3 9)
