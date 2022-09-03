#lang sicp

(define (make-monitored f)
  (let ((cnt 0))
    (define (how-many?) cnt)
    (define (reset) (set! cnt 0))
    (define (dispatch arg)
      (cond ((eq? arg 'how-many?) (how-many?))
            ((eq? arg 'reset) (reset))
            (else 
              (begin 
                (set! cnt (+ cnt 1)) 
                (f arg)))))
    dispatch))


(define s (make-monitored sqrt))
(s 100)
(s 4)
(s 25)
(s 'how-many?)
(s 'reset)
; Check that counter = 0
(s 'how-many?)
(s 1)
; Check that counter increasing
(s 'how-many?)
