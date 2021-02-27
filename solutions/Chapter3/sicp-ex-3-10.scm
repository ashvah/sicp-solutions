#lang sicp

(define (make-withdraw initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

; -> global: make-withdraw  
;               |       ^
;               v       |
;         (variable:   (*)
;         initial-amount)
;            (code)

(define W1 (make-withdraw 100))

; -> global:  make-withdraw ...
;        W1                                         ^
;        |                                          |     
;        v                                          |
;  (variable: (*)                                   |
;    amount)    --> E2 (balance: 100)  -->E1 (initial-amount: 100) 
;    (code)

(W1 50)

; -> global:  make-withdraw ...
;        W1                                         ^                              
;        |                                          |                              
;        v                                          |                              
;  (variable: (*)                                   |                              
;    amount)    --> E2 (balance: 100)  -->E1 (initial-amount: 100)         
;    (code)             ^
;                       |
;                   E3 (amount: 50)

; -> global:  make-withdraw ...
;        W1                                        ^                              
;        |                                         |                              
;        v                                         |                              
;  (variable: (*)                                  |                              
;    amount)    --> E2 (balance: 50)  -->E1 (initial-amount: 100)         
;    (code)             

(define W2 (make-withdraw 100))

; -> global:  make-withdraw ...
;        W1                                        ^                              W2                                          ^              
;        |                                         |                              |                                           | 
;        v                                         |                              v                                           | 
;  (variable: (*)                                  |                           (variable: (*)                                 |   
;    amount)    --> E2 (balance: 50)  -->E1 (initial-amount: 100)                 amount)    --> E5 (balance: 100)  -->E4 (initial-amount: 100)
;    (code)                                                                       (code)



; E1 and E4 are redundant since we never change initial-amount
