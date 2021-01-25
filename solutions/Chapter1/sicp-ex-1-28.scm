#lang sicp
(define (square x) (* x x))     

(define (prime? n)
  (define (expmod base exp)
    (define (check)
      (and (not (= base 1))
           (not (= base (- n 1)))
           (= 1 (remainder (square base) n)))) 
    (cond ((= exp 0) 1)
        ((check) 0)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2)))
                    n))
        (else
         (remainder (* base (expmod base (- exp 1)))
                    n))))     
  (define (fermat-test)
    (define (try-it a)
      (= (expmod a (- n 1)) 1))
    (try-it (+ 1 (random (- n 1)))))
  (define (fast-prime? times)
    (cond ((= times 0) true)
        ((fermat-test) (fast-prime? (- times 1)))
        (else false)))
  (fast-prime? 20))

(display "check by Rabin-test:")
(newline)
(prime? 561)
; #f
(prime? 1105)
; #f
(prime? 1729)
; #f
(prime? 2465)
; #f
(prime? 2821)
; #f
(prime? 6601)
; #f

(define (print n)
  (cond ((= n 1) (display "print primes less than 100:"))
        ((prime? n) (begin
                      (print (- n 1))
                      (newline)
                      (display n)))
        (else (print (- n 1)))))

(print 100)
