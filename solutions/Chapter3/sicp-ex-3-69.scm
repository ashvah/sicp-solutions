#lang sicp

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (sub-stream-even s)
  (let ((rest (stream-cdr s)))
    (cons-stream (stream-car rest)
                 (sub-stream-even (stream-cdr rest)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (triple-pairs s t u)
  (let ((p (pairs t u)))
    (define (iter s t-u-pair)
      (cons-stream
       (cons (stream-car s) (stream-car t-u-pair))
       (interleave (stream-map (lambda (x) (cons (stream-car s) x)) (stream-cdr t-u-pair))
                   (iter (stream-cdr s) (sub-stream-even (stream-cdr t-u-pair))))))
  (iter s p)))

(define (triple S T U)
  (define (double x) (* x x))
  (stream-filter (lambda (pair) (= (+ (double (car pair)) (double (cadr pair))) (double (caddr pair))))
                 (triple-pairs S T U)))

(define triangle (triple integers integers integers))
(display-stream-n triangle 4)
; (3 4 5) (6 8 10) (5 12 13) (9 12 15)
