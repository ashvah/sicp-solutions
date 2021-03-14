#lang sicp

(define (stream-limit s tolerence)
  (define (iter term rest)
    (let ((next-term (stream-car rest)))
      (if (< (abs (- term next-term)) tolerence)
          next-term
          (iter next-term (stream-cdr rest)))))
  (iter (stream-car s) (stream-cdr s)))
