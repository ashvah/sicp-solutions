#lang sicp

(define (make-account balance pwd)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (pwd-error amount)
    (display "Incorrect password\n"))
  (define (dispatch input m)
    (cond ((not (eq? pwd input)) pwd-error)
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

(define (make-joint account pwd new-pwd)
  (define (pwd-error amount)
    (display "Incorrect password\n"))
  (define (dispatch input m)
    (if (eq? input new-pwd)
        (account pwd m)
        pwd-error))
  dispatch)

(define peter-acc (make-account 100 'open-sesame))
((peter-acc 'open-sesame 'withdraw) 40)
; 60
((peter-acc 'some-other-password 'deposit) 50)
; Incorrect password
(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))
((paul-acc 'rosebud 'deposit) 20)
; 80
((paul-acc '12345 'deposit) 20)
; Incorrect password
((peter-acc 'open-sesame 'withdraw) 5)
; 75
((paul-acc 'rosebud 'deposit) 200)
; 275
