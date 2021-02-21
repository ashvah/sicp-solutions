#lang sicp

(define (div-terms L1 L2)
  (if (empty-termlist? L1)
      (list (the-empty-termlist) (the-empty-termlist))
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        (if (> (order t2) (order t1))
            (list (the-empty-termlist) L1)
            (let ((new-c (div (coeff t1) (coeff t2)))
                  (new-o (- (order t1) (order t2))))
              (let ((rest-of-result
                     (div-terms (sub-terms L1 (mul-terms (make-term new-o new-c) L2)) L2)
                     ))
                (list (adjoin-term (make-term new-o new-c) (car rest-of-result)) (cadr rest-of-result))
                ))))))

(define (div-poly P1 P2)
  (let ((v1 (variable P1))
        (v2 (variable P2)))
    (if (= v1 v2)
        (map (lambda (tl) (make-poly v1 tl)) (div-terms (term-list P1) (term-list P2)))
        (error "The variables are different --DIV-POLY" (list P1 P2)))))
