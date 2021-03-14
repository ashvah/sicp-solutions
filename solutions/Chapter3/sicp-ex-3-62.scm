#lang sicp

(define (div-series s1 s2)
  (if (= 0 (stream-car s2))
      (error "DIV-ZERO --DIV-SERIES")
      (mul-series s1 (reverse-series s2))))

(define tangent-series (div-series sine-series cosine-series))
(display-stream-n tangent-series 10)
; 0 1 0 1/3 0 2/15 0 17/315 0 62/2835 
