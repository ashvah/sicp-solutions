#lang sicp

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (interleave
     (stream-map (lambda (x) (list x (stream-car t))) (stream-cdr s))
     (pairs (stream-cdr s) (stream-cdr t))))))

(define p (pairs integers integers))

(display-stream-n p 15)
; (1 1) (1 2) (2 1) (1 3) (2 2) (1 4) (3 1) (1 5) (2 3) (1 6) (4 1) (1 7) (3 2) (1 8) (5 1) 
