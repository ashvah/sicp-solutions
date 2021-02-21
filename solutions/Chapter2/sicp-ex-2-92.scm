#lang sicp

(define (add-poly p1 p2)
  (let ((vs1 (variables p1))
        (vs2 (variables p2)))
    (let ((v1 (car vs1))
          (v2 (car vs2)))
      (cond ((same-variables? v1 v2) (make-poly (union (variable p1) (variable p2))
                                                (add-terms (term-list p1)
                                                           (term-list p2))))
            ((greater? v1 v2) (make-poly (union (variable p1) (variable p2))
                                         (add-terms (term-list p1)
                                                    (make-term 0 (tag p2)))))
            (else (make-poly (union (variable p1) (variable p2))
                             (add-terms (make-term 0 (tag p1))
                                        (term-list p2))))))))


(define (mul-poly p1 p2)
  (let ((vs1 (variables p1))
        (vs2 (variables p2)))
    (let ((v1 (car vs1))
          (v2 (car vs2)))
      (cond ((same-variables? v1 v2) (make-poly (union (variable p1) (variable p2))
                                                (mul-terms (term-list p1)
                                                           (term-list p2))))
            ((greater? v1 v2) (make-poly (union (variable p1) (variable p2))
                                         (mul-terms (term-list p1)
                                                    (make-term 0 p2))))
            (else (make-poly (union (variable p1) (variable p2))
                             (mul-terms (make-term 0 p1)
                                        (term-list p2))))))))

;; *incomplete* skeleton of package
(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variables term-list)
    (list variables term-list))
  (define (variables p) (car p))
  (define (term-list p) (cadr p))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))

  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)

(define (add arg1 arg2)
  (let ((t1 (type-tag arg1))
        (t2 (type-tag arg2))
        (c1 (content arg1))
        (c2 (content arg2)))
    (cond ((and (eq? t1 'polynomial)
                (eq? t2 'polynomial)) ((get 'add '(polynomial polynomial)) c1 c2))
          ((eq? t1 'polynomial) (make-poly (variables c1) (add-terms (term-list c1) (make-term 0 arg2))))
          ((eq? t2 'polynomial) (make-poly (variables c2) (add-terms (term-list c2) (make-term 0 arg1))))
          (else (apply-generic 'add arg1 arg2)))))

(define (mul arg1 arg2)
  (let ((t1 (type-tag arg1))
        (t2 (type-tag arg2))
        (c1 (content arg1))
        (c2 (content arg2)))
    (cond ((and (eq? t1 'polynomial)
                (eq? t2 'polynomial)) ((get 'mul '(polynomial polynomial)) c1 c2))
          ((eq? t1 'polynomial) (make-poly (variables c1) (mul-terms (term-list c1) (make-term 0 arg2))))
          ((eq? t2 'polynomial) (make-poly (variables c2) (mul-terms (term-list c2) (make-term 0 arg1))))
          (else (apply-generic 'mul arg1 arg2)))))

(define (add-terms L1 L2)
  (cond ((empty-termlist? L1) L2)
        ((empty-termlist? L2) L1)
        (else
         (let ((t1 (first-term L1)) (t2 (first-term L2)))
           (cond ((> (order t1) (order t2))
                  (adjoin-term
                   t1 (add-terms (rest-terms L1) L2)))
                 ((< (order t1) (order t2))
                  (adjoin-term
                   t2 (add-terms L1 (rest-terms L2))))
                 (else
                  (adjoin-term
                   (make-term (order t1)
                              (add (coeff t1) (coeff t2)))
                   (add-terms (rest-terms L1)
                              (rest-terms L2)))))))))

(define (mul-terms L1 L2)
  (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))

(define (mul-term-by-all-terms t1 L)
  (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
         (make-term (+ (order t1) (order t2))
                    (mul (coeff t1) (coeff t2)))
         (mul-term-by-all-terms t1 (rest-terms L))))))


;; Representing term lists

(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (cons term term-list)))

(define (the-empty-termlist) '())
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))

(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))


;; Constructor
(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))
