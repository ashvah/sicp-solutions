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

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
        (apply proc (map stream-car argstreams))
        (apply stream-map
               (cons proc (map stream-cdr argstreams))))))

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

(define (stream-fetch-n proc s n)
  (define (iter rest n)
    (if (or (stream-null? rest) (= n 0))
        (display "\n")
        (begin (proc (stream-car rest))
               (iter (stream-cdr rest) (- n 1)))))
  (iter s n))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-stream-n s n)
  (stream-fetch-n display s n))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

;----------------------------------------------------------
; some useful streams
(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

;---------------------------------------------------------------------
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

(display-stream-n (expand 1 7 10) 20)
; 1 4 2 8 5 7 ...
(display-stream-n (expand 3 8 10) 20)
; 3 7 5 0 0 0 ...
