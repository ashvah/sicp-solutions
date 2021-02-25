#lang sicp

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral trials P x1 x2 y1 y2)
  (define (test)
    (let ((x (random-in-range x1 x2))
          (y (random-in-range y1 y2)))
      (P x y)))
  (* (monte-carlo trials test) (- x2 x1) (- y2 y1)))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(* 4 (estimate-integral 10000 (lambda (x y) (<= (+ (* x x) (* y y)) 1)) 0 1.0 0 1.0))
; 3.158
