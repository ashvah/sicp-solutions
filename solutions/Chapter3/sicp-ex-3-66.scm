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
    (pairs (stream-cdr s) (stream-cdr t)))))

(define p (pairs integers integers))

(display-stream-n p 10)
; (1 1) (1 2) (2 2) (1 3) (2 3) (1 4) (3 3) (1 5) (2 4) (1 6) 
; f(n) reprensents the n-th element
; f(1)=(1 1)
; if n%2==0 f(n)=(1 n/2+1)
; if n%2==1 f(n)=f(n/2)+(1 1)
; k=(a_n a_{n-1} ... a_2 a_1 a_0)_2
; if m is the smallest number s.t. a_m=0
; f(k)=(m+1,(a_n a_{n-1}... a_{m+1})_2+m+1)
; e.g.
; 198=(11000110)_2
; f(198)=(0+1, (1100011)_2+0+1)=(1 100)
(stream-ref p 197)
; 131=(10000011)_2
; f(131)-(2+1,(10000)_2+2+1)=(3 19)
(stream-ref p 130)
; 2^100-1=(0111111111111)_2
; f(2^100-1)=(99+1,(0)_2+99+1)=(100 100)
