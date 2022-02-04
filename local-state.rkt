#lang sicp

(define (make-accumulator initial)
  (lambda (x)
    (begin (set! initial (+ initial x))
           initial)))


(define (make-monitored func)
  (let ((cnt 0))
    (lambda (arg) 
      (cond ((eq? arg 'how-many-calls) 
              cnt)
            ((eq? arg 'reset-count)
              (set! cnt 0))
            (else (begin (set! cnt (inc cnt))
                         (func arg)))))))
            
(define (make-account balance password)
  (let ((incorrect-cnt 0))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount)) 
                balance)
          "Insufficient funds"))
    
    (define (deposit amount)
      (begin (set! balance (+ balance amount))
              balance))
    
    (define (call-cops)
      (display "Police is called!!!"))
    
    (define (dispatch m pwd)
      (if (eq? pwd password)
          (begin (set! incorrect-cnt 0)
                 (cond ((eq? m 'withdraw) withdraw)
                  ((eq? m 'deposit) deposit)
                  (else (error "Unknown request -- MAKE-ACCOUNT"  m))))
          (if (< incorrect-cnt 7)
            (begin (set! incorrect-cnt (inc incorrect-cnt))
                   (error "Incorrect password -- MAKE-ACCOUNT" pwd))
            (call-cops))))

    dispatch))
