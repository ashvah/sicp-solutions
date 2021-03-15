#lang sicp

(define (smooth s)
  (define (aver x y)
    (/ (+ x y) 2))
  (stream-map aver s (stream-cdr s)))

(define (zero-crossings stream smooth)
  (let ((after (smooth stream)))
    (stream-map sign-change-detector (stream-cdr after) after)))
