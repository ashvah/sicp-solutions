#lang sicp

; account: balance
;          (list acc1 acc2 acc3 ...)
;          (list amount1 amount2 amount3 ...)

(define (proc acc)
  (define (iter l1 l2)
    (if (null? l2)
        'done
        (begin
          ((acc 'withdraw) (car l2))
          (((car l1) 'deposit) (car (l2)))
          (iter (cdr l1) (cdr l2)))))
  (let ((acc-list (acc 'get-acc-list))
        (amount-list (acc 'get-amount-list)))
    (iter acc-list amount-list)))

; acc1: 30 (list acc2) (list 20)
; acc2: 50 (list acc1) (list 40)
; (proc acc1)
; (proc acc2)
