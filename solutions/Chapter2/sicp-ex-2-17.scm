#lang sicp

; Since the exercise limits the list to be non-empty 
; I won't check whether x is empty in this function
(define (last-pair x)
  (let ((y (cdr x)))
       (if (null? y)
           x
           (last-pair y))))

(last-pair (list 23 72 149 34))
; (34)
