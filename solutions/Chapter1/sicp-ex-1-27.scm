#lang sicp
(define (square x) (* x x))

(define (false-prime? n)
  (define (expmod base exp m)
    (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m)))) 
  (define (fermat-test a)
    (= (expmod a n n) a))
  (define (fast-prime? a)
    (cond ((= a n) true)
        ((fermat-test a) (fast-prime? (+ a 1)))
        (else false)))
  (fast-prime? 1))


(define (true-prime? n)
  (define (smallest-divisor n)
    (find-divisor n 2))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
  (define (divides? a b)
    (= (remainder b a) 0))
  (define factor n)
  (begin
    (set! factor (smallest-divisor n))
    (display (= n factor))
    (newline)
    (display n)
    (display "=")
    (display factor)
    (display "*")
    (display (/ n factor))
    (newline)))


(display "check by fermat-test:")
(newline)
(false-prime? 561)
; #t
(false-prime? 1105)
; #t
(false-prime? 1729)
; #t
(false-prime? 2465)
; #t
(false-prime? 2821)
; #t
(false-prime? 6601)
; #t

(display "check by divisors:")
(newline)
(true-prime? 561)
; #f
; 561=3*187
(true-prime? 1105)
; #f
; 1105=5*221
(true-prime? 1729)
; #f
; 1729=7*247
(true-prime? 2465)
; #f
; 2465=5*493
(true-prime? 2821)
; #f
; 2821=7*403
(true-prime? 6601)
; #f
; 6601=7*943
