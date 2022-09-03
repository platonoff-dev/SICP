#lang racket/base

;; 3.7 Suppose that our banking system requires the ability to make joint accounts. 
;; Define a procedure make-joint that accomplishes this. ...

(define (make-account balance password)
  (let ([incorrect-count 0] [registered-passwords (list password)])
    (define (call-the-cops) "Calling the police!!!!!!")

    (define (check-password pwd)
      (define (check-password-rec pass allowed-list)
        (if (null?(car allowed-list))
          (if (> incorrect-count 6) 
            (call-the-cops)
            (begin
              (set! incorrect-count (+ incorrect-count 1))
              "Incorrect password! Try again!"))
        (if (eq? (car allowed-list) pass)
          (begin
            (set! incorrect-count 0)
            null)
          (check-password-rec pass (cdr allowed-list)))))
      (check-password-rec pwd registered-passwords))

    (define (withdraw ammount)
      (if (>= balance ammount)
        (begin
          (set! balance (- balance ammount))
          balance)
        "Insufficient funds!"))

    (define (deposit ammount)
      (set! balance (+ balance ammount))
      balance)
    
    (define (add-password pass)
      (set! registered-passwords (cons registered-passwords pass)))

    (define (dispatch pass action)
      (let ([pass-check-result (check-password pass)])
        (if (not (null? pass-check-result))
          (lambda (_) pass-check-result)
          (cond ((eq? action 'withdraw) withdraw)
                ((eq? action 'deposit) deposit)
                ((eq? action 'add-password) add-password)
                (else (error "Unknown request -- MAKE-ACCOUNT" action))))))
    dispatch))

(define (make-joint acc pass new-pass)
  (begin
    ((acc pass 'add-password) new-pass)
    acc))

(define acc (make-account 100 'pass))
((acc 'pass 'deposit) 100)
((acc 'pass 'withdraw) 150)

(define acc2 (make-joint acc 'pass 'new-pass))
((acc2 'new-pass 'deposit) 300)
