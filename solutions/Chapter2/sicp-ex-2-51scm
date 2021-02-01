#lang sicp

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-down
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              (make-vect 1.0 0.0)
                              split-point))
          (paint-up
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-up frame)
        (paint-down frame)))))
        
(define (below p1 p2)
  (rotate270 (beside (rotate90 p2) (rotate90 p1))))
