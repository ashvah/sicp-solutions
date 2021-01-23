#lang sicp
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (square x)
  (* x x))

(sqrt 9)
; out of memory
; my interpreter use applicative-order evaluation
; 'if' is special form which only evaluates one clause, even in applicative-order evaluation
; but 'new-if' evaluates both two clauses so it need to evaluate sqrt-iter as else-clause, which enters an infinite loop
; if we don't define 'new-if' and use 'cond' directly, we can get the true result

(define (new-sqrt-iter guess x)
  (cond ((good-enough? guess x) guess)
        (else (new-sqrt-iter (improve guess x) x))))

(define (new-sqrt x)
  (new-sqrt-iter 1.0 x))

(new-sqrt 9)
; 3.00009155413138
