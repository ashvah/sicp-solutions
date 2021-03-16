#lang sicp

(define (solve-2nd a b dt y0 dy0)
  (define ddy (add-streams (scale-stream (cons-stream 0 dy) a) (scale-stream (cons-stream 0 y) b)))
  (define dy (integral (delay (stream-cdr ddy)) dy0 dt))
  (define y (integral (delay (stream-cdr dy)) y0 dt))
  y)

(define f (solve-2nd 2 -1 0.001 1 2))
; f(t)=exp(t)(1+t)
(stream-ref f 1000)
; 5.436113232761698
(* 2 (exp 1))
; f(1)=2e=5.43656365691809
(stream-ref f 2000)
; 22.135235783975116
(* 3 (exp 2))
; f(2)=3*e^2=22.16716829679195
