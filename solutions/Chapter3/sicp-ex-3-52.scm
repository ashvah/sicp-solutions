#lang sicp

;----------------------------------------
; stream
(define (stream-car s) (car s))
(define (stream-cdr s) (force (cdr s)))
(define the-empty-stream nil)

;----------------------------------------
; stream operation
(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (display-line x)
  (display x)
  (newline))

(define (show x)
  (display-line x)
  x)

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

;---------------------------------------------------------------------

(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))
(display "sum=")
sum
; sum=1
; (cons 1 (delay ...))
(define y (stream-filter even? seq))
(display "sum=")
sum
; sum=3
; (cons 3 (delay ...))
; sum=6
; (cons 6 (delay ...))
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))
(display "sum=")
sum
; (cons 1 (delay...))
; (cons 3 (delay...))
; (cons 6 (delay...))
; sum=10
; (cons 10 (delay ...))

(stream-ref y 7)
(display "sum=")
sum
; (stream-ref (6 10 ...) 7)
; (stream-ref (10 ...) 6)
; sum=15
; sum=21
; sum=28
; (stream-ref (28 ...) 5)
; sum=36
; (stream-ref (36 ...) 4)
; sum=45
; sum=55
; sum=66
; (stream-ref (66 ...) 3)
; sum=78
; (stream-ref (78 ...) 2)
; sum=91
; sum=105
; sum=120
; (stream-ref (120 ...) 1)
; sum=136
; (stream-ref (136 ...) 0)
; 136

(display-stream z)
(display "sum=")
sum
; 10
; 15
; 45
; 55
; 105
; 120
; sum=153
; sum=171
; sum=190
; 190
; sum=210
; 210
