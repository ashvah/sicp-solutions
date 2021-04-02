#lang sicp

(display "Start testing:\n")
(actual-value '(define (fib n)
                  (if (= n 1)
                      1
                      (if (= n 0)
                          1
                          (+ (fib (dec n)) (fib (dec (dec n))))))) the-global-environment)
(actual-value '(define (f x) (+ x x x x)) the-global-environment)
(define time (runtime))
(actual-value '(f (fib 24)) the-global-environment)
(display (- (runtime) time))

; memory: 690152
; non: 10625534
; 6%

(define count 0)

(define (id x)
  (set! count (+ count 1))
  x)
(define (square x)
  (* x x))

(square (id 10))
count

; memory: 1
; non: 2
