#lang sicp

(define (ripple-carry-adder A B S C)
  (if (null? A)
      (set-signal! C 0)
      (begin
        (let ((c-in (make-wire)))
          (ripple-carry-adder (cdr A) (cdr B) (cdr S) c-in)
          (full-adder (car A) (car B) c-in (car S) C)))))
