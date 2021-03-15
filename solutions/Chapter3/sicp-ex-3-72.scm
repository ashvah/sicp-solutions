#lang sicp

(define (double x) (* x x))

(define (weight p)
  (+ (double (car p)) (double (cadr p))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted weight
                   (stream-map (lambda (x) (list (stream-car s) x))
                               (stream-cdr t))
                   (pairs (stream-cdr s) (stream-cdr t)))))

(define result
  (let ((s (pairs integers integers)))
    (define (iter old_sum rest)
      (if (= (weight (stream-car rest)) old_sum)
          (iter old_sum (stream-cdr rest))
          (let ((first (stream-car rest))
                (second (stream-car (stream-cdr rest)))
                (third (stream-car (stream-cdr (stream-cdr rest))))
                (fourth (stream-car (stream-cdr (stream-cdr (stream-cdr rest))))))
            (let ((w1 (weight first))
                  (w2 (weight second))
                  (w3 (weight third))
                  (w4 (weight fourth)))
              (if (= w1 w2)
                  (if (= w2 w3)
                      (if (= w3 w4)
                          (iter w4 (stream-cdr (stream-cdr (stream-cdr (stream-cdr rest)))))
                          (cons-stream (list w1 first second third)
                                       (iter w3 (stream-cdr (stream-cdr (stream-cdr rest))))))
                      (iter w2 (stream-cdr (stream-cdr rest))))
                  (iter w1 (stream-cdr rest)))))))
    (iter 0 s)))

(display-stream-n result 10)
; (325 (1 18) (6 17) (10 15))
; (425 (5 20) (8 19) (13 16))
; (650 (5 25) (11 23) (17 19))
; (725 (7 26) (10 25) (14 23))
; (845 (2 29) (13 26) (19 22))
; (850 (3 29) (11 27) (15 25))
; (925 (5 30) (14 27) (21 22))
; (1025 (1 32) (8 31) (20 25))
; (1250 (5 35) (17 31) (25 25))
; (1300 (2 36) (12 34) (20 30)) 
