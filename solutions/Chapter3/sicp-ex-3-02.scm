#lang sicp

(define (make-monitored f)
  (let ((call-times 0))
    (lambda (x)
      (cond ((eq? x 'how-many-calls?) call-times)
            ((eq? x 'reset-count) (set! call-times 0))
            (else (set! call-times (inc call-times)) (f x))))))

(define s (make-monitored sqrt))
(s 100)
; 10
(s 'how-many-calls?)
; 1
(s 36)
; 6
(s 'how-many-calls?)
; 2
(s 'reset-count)
(s 'how-many-calls?)
; 0
