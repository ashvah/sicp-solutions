#lang sicp

;---------------------------------------------------------
; test
(display "Start testing:\n")
(eval '(define (fib n)
         (if (= n 1)
             1
             (if (= n 0)
                 1
                 (+ (fib (- n 1)) (fib (- n 2))))))
      the-global-environment)
(define start-time (runtime))
(set! start-time (runtime))
(eval '(fib 25) the-global-environment)
(display (- (runtime) start-time))
; anlyze ~1184740
; origin ~2171320
; performance improved about 50%
