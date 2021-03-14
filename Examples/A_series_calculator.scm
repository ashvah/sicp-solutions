#lang sicp

;----------------------------------------
; stream definition

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
  (stream-fetch-n (lambda (x) (display x) (display " ")) s n))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

;---------------------------------------------
; series operation

(define (integrate-series series)
  (define (iter s n)
    (if (null? s)
        'done
        (cons-stream (/ (stream-car s) n) (iter (stream-cdr s) (inc n)))))
  (iter series 1))

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (mul-series s1 (stream-cdr s2)) (scale-stream (stream-cdr s1) (stream-car s2)))))

(define (reverse-series s)
  (define result
    (cons-stream 1 (mul-series result (scale-stream (stream-cdr s) -1))))
  result)

(define (div-series s1 s2)
  (if (= 0 (stream-car s2))
      (error "DIV-ZERO --DIV-SERIES")
      (mul-series s1 (reverse-series s2))))

(define (diff-series s)
  (define (iter series n)
    (cons-stream (* (stream-car series) n) (iter (stream-cdr series) (inc n))))
  (iter (stream-cdr s) 1))

(define (add-series s1 s2)
  (add-streams s1 s2))

(define (sub-series s1 s2)
  (add-streams s1 (scale-stream s2 -1)))

;----------------------------------------------------------
; some useful streams

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))
(define cosine-series
  (cons-stream 1 (scale-stream (integrate-series sine-series) -1)))
(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))
(define tangent-series (div-series sine-series cosine-series))
;---------------------------------------------------------------------
; test

(display-stream-n sine-series 10)
; 0 1 0 -1/6 0 1/120 0 -1/5040 0 1/362880 
(display-stream-n cosine-series 10)
; 1 0 -1/2 0 1/24 0 -1/720 0 1/40320 0
(display-stream-n tangent-series 10)
; 0 1 0 1/3 0 2/15 0 17/315 0 62/2835
(display-stream-n (diff-series sine-series) 10)
; 1 0 -1/2 0 1/24 0 -1/720 0 1/40320 0
(display-stream-n (diff-series cosine-series) 10)
; 0 -1 0 1/6 0 -1/120 0 1/5040 0 -1/362880
(display-stream-n (sub-series exp-series (diff-series exp-series)) 10)
; 0 0 0 0 0 0 0 0 0 0
