#lang sicp

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral P x1 x2 y1 y2)
  (define (random-stream) (cons-stream (cons (random-in-range x1 x2)
                                           (random-in-range y1 y2))
                                     (random-stream)))
  (stream-map (lambda (p) (* p (- x2 x1) (- y2 y1))) (monte-carlo (stream-map (lambda (x) (P (car x) (cdr x))) (random-stream)) 0 0)))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define result (scale-stream (estimate-integral (lambda (x y) (<= (+ (* x x) (* y y)) 1)) 0 1.0 0 1.0) 4))
(stream-ref result 10000)
; 3.148885111488851
