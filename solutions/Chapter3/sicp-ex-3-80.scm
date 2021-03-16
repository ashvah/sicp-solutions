#lang sicp


(define (RLC R L C dt)
  (lambda (vc0 il0)
    (define dil (add-streams (scale-stream (cons-stream 0 il) (- (/ R L))) (scale-stream (cons-stream 0 vc) (/ 1.0 L))))
    (define il (integral (delay (stream-cdr dil)) il0 dt))
    (define dvc (scale-stream (cons-stream 0 il) (/ -1.0 C)))
    (define vc (integral (delay (stream-cdr dvc)) vc0 dt))
    (cons vc il)))

(define RLC1 (RLC 1 1 0.2 0.1))
(define result (RLC1 10 0))

(stream-ref (car result) 10)
; -3.5160605800000004
(* (exp -0.5) (+ (* 10 (cos (/ (sqrt 19) 2))) (* (/ 10 (sqrt 19)) (sin (/ (sqrt 19) 2)))))
; -2.3263241004842534
(stream-ref (cdr result) 10)
; 2.750945989
(* (/ 20 (sqrt 19)) (exp -0.5) (sin (/ (sqrt 19) 2)))
; 2.2831875154500216
