#lang sicp

(define (make-monitored f)
  (let ((call-times 0))
    (lambda (x)
      (if (eq? x 'how-many-calls?)
          call-times
          (begin
            (set! call-times (inc call-times))
            (f x))))))

(define s (make-monitored sqrt))
(s 100)
; 10
(s 'how-many-calls?)
; 1
(s 36)
; 6
(s 'how-many-calls?)
; 2
