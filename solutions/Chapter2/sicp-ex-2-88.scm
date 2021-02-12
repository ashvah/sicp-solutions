#lang sicp

(define (minus p)
  (define (minus-each-term terms)
    ((if (empty-list? tl)
        tl
        (let ((term (first-term tl))
              (rest (rest-term tl)))
          (adjoin-term (make-term (order term) (minus (coeff term))) (minus-each-term rest))))))
  (let ((tl (term-list p))
        (v (variable p)))
    (make-poly v (minus-each-term tl))))

(put 'minus 'scheme-number (lambda (n) (tag (- n))))
(put 'minus 'rational (lambda (n) (tag (make-rat (minus (numer n)) (denom n)))))
(put 'minus 'complex (lambda (n) (tag (make-from-real-imag (minus (real-part n)) (minus (imag-part n))))))
(put 'minus 'polynonomial (lambda (p) (tag (minus p))))
(define (minus n)
  (apply-generic 'minus n))
(put 'sub 'polynomial (lambda (p1 p2) (tag (add-poly p1 (minus p2)))))
