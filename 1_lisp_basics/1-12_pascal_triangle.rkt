#lang racket

(define (pascal row column)
    (if (or (= row column) (= column 1))
        1
        (+ (pascal (- row 1) column) (pascal (- row 1) (- column 1)))
    )
)

(define n 30)

(for ([i (in-range 1 (+ n 1))])
    (for ([j (in-range 1 (+ i 1))])
        (display (string-append (number->string (pascal i j)) " "))
    )
    (display "\n")
)
