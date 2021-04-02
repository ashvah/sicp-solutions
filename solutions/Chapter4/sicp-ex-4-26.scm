#lang sicp

; unless
(define (unless-pred exp)
  (cadr exp))

(define (unless-alter exp)
  (if (not (null? (cadddr exp)))
      (cadddr exp)
      'false))

(define (unless-cons exp)
  (caddr exp))

(define (unless->if exp)
  (make-if (unless-pred exp) (unless-alter exp) (unless-cons exp)))

(define (analyze-unless exp)
  (analyze (unless->if exp)))
