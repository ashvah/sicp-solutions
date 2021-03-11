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

(define (display-line x)
  (display x)
  (newline))

(define (show x)
  (display-line x)
  x)

;---------------------------------------------------------------------

(define x (stream-map show (stream-enumerate-interval 0 10)))
; (cons 0 (delay (stream-enumerate-interval 1 10)))
; (cons (show 0) (delay (stream-map show (stream-enumerate-interval 1 10))))
; 0
; (cons 0 (delay (stream-map show (stream-enumerate-interval 1 10))))

(stream-ref x 5)
; (stream-ref (stream-map show (stream-enumerate-interval 1 10)) 4)
; (stream-ref (stream-map show (cons 1 (delay (stream-enumerate-interval 2 10)))) 4)
; (stream-ref (cons (show 1) (delay (stream-map show (stream-enumerate-interval 2 10)))) 4)
; 1
; (stream-ref (cons 1 (delay (stream-map show (stream-enumerate-interval 2 10)))) 4)
; (stream-ref (stream-map show (stream-enumerate-interval 2 10)) 3)
; (stream-ref (stream-map show (cons 2 (delay (stream-enumerate-interval 3 10)))) 3)
; (stream-ref (cons (show 2) (delay (stream-map show (stream-enumerate-interval 3 10)))) 3)
; 2
; (stream-ref (cons 2 (delay (stream-map show (stream-enumerate-interval 3 10)))) 3)
; (stream-ref (stream-map show (stream-enumerate-interval 3 10)) 2)
; (stream-ref (stream-map show (cons 3 (delay (stream-enumerate-interval 4 10)))) 2)
; (stream-ref (cons (show 3) (delay (stream-map show (stream-enumerate-interval 4 10)))) 2)
; 3
; (stream-ref (cons 3 (delay (stream-map show (stream-enumerate-interval 4 10)))) 2)
; (stream-ref (stream-map show (stream-enumerate-interval 4 10)) 1)
; (stream-ref (stream-map show (cons 4 (delay (stream-enumerate-interval 5 10)))) 1)
; (stream-ref (cons (show 4) (delay (stream-map show (stream-enumerate-interval 5 10)))) 1)
; 4
; (stream-ref (cons 4 (delay (stream-map show (stream-enumerate-interval 5 10)))) 1)
; (stream-ref (stream-map show (stream-enumerate-interval 5 10)) 0)
; (stream-ref (stream-map show (cons 5 (delay (stream-enumerate-interval 6 10)))) 0)
; (stream-ref (cons (show 5) (delay (stream-map show (stream-enumerate-interval 6 10)))) 0)
; 5
; (stream-ref (cons 5 (delay (stream-map show (stream-enumerate-interval 6 10)))) 0)
; 5

(stream-ref x 7)
; (stream-ref (stream-map show (stream-enumerate-interval 6 10)) 1)
; (stream-ref (stream-map show (cons 6 (delay (stream-enumerate-interval 7 10)))) 1)
; (stream-ref (cons (show 6) (delay (stream-map show (stream-enumerate-interval 7 10)))) 1)
; 6
; (stream-ref (cons 6 (delay (stream-map show (stream-enumerate-interval 7 10)))) 1)
; (stream-ref (stream-map show (stream-enumerate-interval 7 10)) 0)
; (stream-ref (stream-map show (cons 7 (delay (stream-enumerate-interval 8 10)))) 0)
; (stream-ref (cons (show 7) (delay (stream-map show (stream-enumerate-interval 8 10)))) 0)
; 7
; (stream-ref (cons 7 (delay (stream-map show (stream-enumerate-interval 8 10)))) 0)
; 7
