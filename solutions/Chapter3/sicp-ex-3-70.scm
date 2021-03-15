#lang sicp

(define (merge-weighted weight s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((<= (weight s1car) (weight s2car))
                  (cons-stream s1car (merge-weighted weight (stream-cdr s1) s2)))
                 (else
                  (cons-stream s2car (merge-weighted weight s1 (stream-cdr s2)))))))))


; a)
(define (weighted-a p)
  (+ (car p) (cadr p)))

(define (pairs-a s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted weighted-a
                   (stream-map (lambda (x) (list (stream-car s) x))
                               (stream-cdr t))
                   (pairs-a (stream-cdr s) (stream-cdr t)))))

(display-stream-n (pairs-a integers integers) 10)
; (1 1) (1 2) (1 3) (2 2) (1 4) (2 3) (1 5) (2 4) (3 3) (1 6)

; b)
(define (weighted-b p)
  (+ (* 2 (car p))
     (* 3 (cadr p))
     (* 5 (car p) (cadr p))))

(define (pairs-b s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted weighted-b
                   (stream-map (lambda (x) (list (stream-car s) x))
                               (stream-cdr t))
                   (pairs-b (stream-cdr s) (stream-cdr t)))))

(define S (cons-stream 1 (merge (merge (scale-stream S 2) (scale-stream S 5)) (scale-stream S 3))))
(display-stream-n (pairs-b S S) 10)
; (1 1) (1 2) (1 3) (2 2) (1 4) (1 5) (2 3) (1 6) (2 4) (3 3) 
