#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))


(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))


(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
          (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

(define (triple-pair n)
  (flatmap (lambda (x)
             (map (lambda (pair) (cons x pair))
                  (unique-pairs (- x 1))))
                (enumerate-interval 1 n)))

(define (triple-tuple-equal-s n s)
  (define (sum-equal? seq)
    (= s (+ (car seq) (car (cdr seq)) (car (cdr (cdr seq))))))
  (filter sum-equal?
          (triple-pair n)))

(triple-tuple-equal-s 10 14)
; ((6 5 3) (7 4 3) (7 5 2) (7 6 1) (8 4 2) (8 5 1) (9 3 2) (9 4 1) (10 3 1))
