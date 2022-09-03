#lang sicp

;; 3.3 Modify the make-account procedure so that it creates password protected accounts. That is,
;; make-aaccount should take a symbol as and additional argument. The resu;ting account should process
;; request only if it's accompanied by the password with ehich acount was created, and should otherwise
;; return a coplaint: Invalid password
;;
;; 3.4 Modify the make-account procedure of exercise 3.3 by adding another local state variable
;; so that, if an account is accesed more than 7 consecutive times with an incorrect password, 
;; it invokes procedure call-the-cops
;;
;; 
;; Solution for both tasks

(define (make-account balance password)
  (let ((incorrect-count 0))
    (define (call-the-cops) "Calling the police!!!!!!")

    (define (check-password pwd)
      (if (not (eq? pwd password))
        (if (> incorrect-count 6) 
          (call-the-cops)
          (begin
            (set! incorrect-count (inc incorrect-count))
            "Incorrect password! Try again!"))
        (begin
          (set! incorrect-count 0)
          nil)))

    (define (withdraw ammount)
      (if (>= balance ammount)
        (begin
          (set! balance (- balance ammount))
          balance)
        "Insufficient funds!"))

    (define (deposit ammount)
      (set! balance (+ balance ammount))
      balance)

    (define (dispatch pass action)
      (let ((pass-check-result (check-password pass)))
        (if (not (null? pass-check-result))
          (lambda (_) pass-check-result)
          (cond ((eq? action 'withdraw) withdraw)
                ((eq? action 'deposit) deposit)
                (else (error "Unknown request -- MAKE-ACCOUNT" action))))))
    dispatch))

;; Manual checks
(display "\nCheck basic operations\n")
(define acc (make-account 100 'password))
((acc 'password 'deposit) 50)
((acc 'password 'withdraw) 150)
((acc 'password 'withdraw) 10)

(display "\nCheck invalid password\n")
(define acc1 (make-account 0 'password))
((acc1 'invalid 'deposit) 10)
((acc1 'invalid 'withdraw) 100)

(display "\nCheck call the police\n")
(define acc2 (make-account 1000 'password))
((acc2 'invalid 'deposit) 10)
((acc2 'invalid 'deposit) 10)
((acc2 'invalid 'deposit) 10)
((acc2 'invalid 'deposit) 10)
((acc2 'invalid 'deposit) 10)
((acc2 'invalid 'deposit) 10)
((acc2 'invalid 'deposit) 10)
((acc2 'invalid 'deposit) 10)

(display "\nCheck not consecutive\n")
(define acc3 (make-account 1000 'password))
((acc3 'invalid 'deposit) 10)
((acc3 'invalid 'deposit) 10)
((acc3 'invalid 'deposit) 10)
((acc3 'invalid 'deposit) 10)
((acc3 'password 'deposit) 10)
((acc3 'invalid 'deposit) 10)
((acc3 'invalid 'deposit) 10)
((acc3 'invalid 'deposit) 10)
((acc3 'invalid 'deposit) 10)
