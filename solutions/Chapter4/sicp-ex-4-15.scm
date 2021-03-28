#lang sicp

(define (run-forever) (run-forever))

(define (try p)
  (if (halts? p p)
      (run-forever)
      'halted))

(try try)
; (halts? try try)=true: (run-forever) so (halts? try try) is false
; (halts? try try)=false: 'halted so (halts? try try) is true
