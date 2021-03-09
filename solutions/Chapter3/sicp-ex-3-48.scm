#lang sicp

;------------------------------------
; serializer
(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

;------------------------------------
; mutex
(define (make-mutex)
  (let ((cell (list false)))            
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire)))
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell)
  (set-car! cell false))

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))

;------------------------------------
; account
(define make-account-and-serializer
  (let ((init 0))
    (lambda (balance)
      (set! init (inc init))
      (let ((number init))
        (define (withdraw amount)
          (if (>= balance amount)
              (begin (set! balance (- balance amount))
                     balance)
              "Insufficient funds"))
        (define (deposit amount)
          (set! balance (+ balance amount))
          balance)
        (let ((balance-serializer (make-serializer)))
          (define (dispatch m)
            (cond ((eq? m 'withdraw) withdraw)
                  ((eq? m 'deposit) deposit)
                  ((eq? m 'balance) balance)
                  ((eq? m 'serializer) balance-serializer)
                  ((eq? m 'number) number)
                  (else (error "Unknown request -- MAKE-ACCOUNT" m))))
          dispatch)))))

;------------------------------------
; exchange
(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)
    'ok))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer))
        (n1 (account1 'number))
        (n2 (account2 'number)))
    (if (< n1 n2)
        ((serializer1 (serializer2 exchange)) account1 account2)
        ((serializer2 (serializer1 exchange)) account1 account2))))

;------------------------------------
; test
(define acc1 (make-account-and-serializer 30))
(define acc2 (make-account-and-serializer 40))
(acc1 'balance)
; 30
(acc2 'balance)
; 40
(serialized-exchange acc1 acc2)
(acc1 'balance)
; 40
(acc2 'balance)
; 30
