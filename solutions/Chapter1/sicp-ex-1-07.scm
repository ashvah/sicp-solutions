#lang sicp
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
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

(define (square x) (* x x))

(sqrt 0.001)
; 0.04124542607499115
(sqrt 0.0001)
; 0.03230844833048122
(sqrt 0.00001)
; 0.03135649010771716
(sqrt 0.000001)
; 0.031260655525445276
(sqrt 10000000000000000000000000000000000000000000000)
; can't output the result, user break
; in all these examples, we get a wrong answer

; new approach
(define (new-good-enough? guess-new guess-old)
  (< (abs (/ (- guess-new guess-old) guess-old)) 0.001))

(define (new-sqrt-iter guess-new guess-old x)
  (if (new-good-enough?  guess-new guess-old)
      guess-new
      (new-sqrt-iter (improve guess-new x) guess-new x)))

(define (new-sqrt x)
  (new-sqrt-iter 1.0 x x))

(new-sqrt 0.001)
; 0.03162278245070105
(new-sqrt 0.0001)
; 0.010000000025490743
(new-sqrt 0.00001)
; 0.0031622776602038957
(new-sqrt 0.000001)
; 0.0010000001533016628
(new-sqrt 10000000000000000000000000000000000000000000000)
; 1.0000000000631588e+023
; all the results are correct
