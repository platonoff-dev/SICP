#lang racket

(define (f-rec n)
  (if (< n 3)
      n
      (+ (f-rec (- n 1)) (* 2 (f-rec (- n 2))) (* 3 (f-rec (- n 3))))))

(define (f-iter n)
  (define (fiter f_1 f_2 f_3 n)
    (if (> n 3)
        (fiter (+ f_1 (* 2 f_2) (* 3 f_3)) f_1 f_2 (- n 1))
        (+ f_1 (* 2 f_2) (* 3 f_3))))
  (if (< n 3)
      n
      (fiter 2 1 0 n)))

(define n 23)
(display (string-append "Recursive(" (number->string n) "): " (number->string (f-rec n)) " \n"))
(display (string-append "Iterative(" (number->string n) "): " (number->string (f-iter n)) " \n"))
