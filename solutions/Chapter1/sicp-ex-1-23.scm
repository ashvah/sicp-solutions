#lang sicp
(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (next n)
  (if (= n 2) 3 (+ n 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (define (report-prime elapsed-time)
  (display n)
  (display " *** ")
  (display elapsed-time)
  (newline))
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (search-for-primes a b)
  (define (search n)
    (cond ((<= n b) (timed-prime-test n) (search (+ n 2)))))
  (search (if (even? a) (+ a 1) a)))

(search-for-primes 1000 1020)
(newline)
(search-for-primes 10000 10038)
(newline)
(search-for-primes 100000 100045)
(newline)
(search-for-primes 1000000 1000038)
