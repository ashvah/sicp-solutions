#lang sicp

; a)
(define wave
  (segment->painter (list (make-segment (make-vect (...) (...))
                                        (make-vect (...) (...)))
                          (...))))

; b)
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
          (beside (below painter up)
                  (below right corner)))))

; c)

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-vert rotate180
                                  identity flip-horiz)))
    (combine4 (corner-split painter n))))
