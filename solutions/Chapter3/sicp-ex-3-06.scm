#lang sicp

(define random-init 7)

(define (rand-update x)
  (remainder (+ 1 (* 16807 x)) 2147483647))

(define rand
  (let ((x random-init))
    (define generate
      (lambda ()
        (set! x (rand-update x))
        x))
    (define (reset new)
      (set! x new))
    (lambda (op)
      (cond ((eq? op 'generate) (generate))
            ((eq? op 'reset) reset)))))

(rand 'generate)
; 117650
(rand 'generate)
; 1977343551
((rand 'reset) 43)
(rand 'generate)
; 722702
(rand 'generate)
; 1409034280
