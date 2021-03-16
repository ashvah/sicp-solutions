#lang sicp

(define (solve-2nd a b dt y0 dy0 f)
  (define ddy (stream-map f (cons-stream 0 dy) (cons-stream 0 y)))
  (define dy (integral (delay (stream-cdr ddy)) dy0 dt))
  (define y (integral (delay (stream-cdr dy)) y0 dt))
  y)
  
