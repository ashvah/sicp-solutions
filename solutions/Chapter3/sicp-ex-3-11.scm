#lang sicp

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

; --> global:   make-account
;                   |           ^
;                   v           |
;          (variable: balance) (*)
;             (define ...)
;             (define ...)
;             (define ...)
;              dispatch

(define acc (make-account 50))

; --> global:   make-account ...
;                  acc
;                   |                        ^
;                   v                        |
;                  (*)(*)       ---> E1(balance 50)
;                   |                   withdraw ...
;                   |                   deposit ...
;                   |---------------->  dispatch ...

((acc 'deposit) 40)

; --> global:   make-account ...
;                  acc
;                   |                        ^                       
;                   v                        |                       
;                  (*)(*)       ---> E1(balance 50)   <---  E2(m: 'deposit) 
;                   |                   withdraw ...            
;                   |                   deposit ...
;                   |---------------->  dispatch ...   <-- E3(amount: 40)

; --> global:   make-account ...
;                  acc
;                   |                        ^
;                   v                        |
;                  (*)(*)       ---> E1(balance 90)
;                   |                   withdraw ...
;                   |                   deposit ...
;                   |---------------->  dispatch ...

((acc 'withdraw) 60)

; --> global:   make-account ...
;                  acc
;                   |                        ^                       
;                   v                        |                       
;                  (*)(*)       ---> E1(balance 90)  <---  E4(m: 'withdraw)
;                   |                   withdraw ...            
;                   |                   deposit ...
;                   |---------------->  dispatch ...  <--- E5(amount: 60)

; --> global:   make-account ...
;                  acc
;                   |                        ^
;                   v                        |
;                  (*)(*)       ---> E1(balance 30)
;                   |                   withdraw ...
;                   |                   deposit ...
;                   |---------------->  dispatch ...

(define acc2 (make-account 100))

; --> global:   make-account ...
;                  acc
;                   |                        ^
;                   v                        |
;                  (*)(*)       ---> E1(balance 30)
;                   |                   withdraw ...
;                   |                   deposit ...
;                   |---------------->  dispatch ...
;
;                  acc2
;                   |                        ^
;                   v                        |
;                  (*)(*)       ---> E1(balance 100)
;                   |                   withdraw ...
;                   |                   deposit ...
;                   |---------------->  dispatch ...
