#lang sicp

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define (no-more? x)
  (null? x))

(define (except-first-denomination x)
  (cdr x))

(define (first-denomination x)
  (car x))

(define us-coins (list 50 25 10 5 1))

(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(cc 100 us-coins)
; 292
(cc 100 uk-coins)
; 104561

(define us-coins-reverse (list 1 5 10 25 50))
(define uk-coins-reverse (list 100 50 20 10 5 2 1 0.5))
(cc 100 us-coins-reverse)
; 292
(cc 100 uk-coins-reverse)
; 104561
; the results are the same
