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

(define (display-stream s)
  (stream-for-each display-line s))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

;----------------------------------------------------------
; some useful streams
(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

;---------------------------------------------------------------------

(define (partial-sums s)
 (define result
   (add-streams s
                (cons-stream 0 result)))
  result)

(stream-ref (partial-sums integers) 9999)

(define (partial-sums2 st)
  (define (iter s n)
    (let ((result (+ (stream-car s) n)))
      (cons-stream result (iter (stream-cdr s) result))))
  (iter st 0))
(stream-ref (partial-sums2 integers) 9999)
