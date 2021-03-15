#lang sicp

(define (triple x) (* x x x))

(define (weight p)
  (+ (triple (car p)) (triple (cadr p))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted weight
                   (stream-map (lambda (x) (list (stream-car s) x))
                               (stream-cdr t))
                   (pairs (stream-cdr s) (stream-cdr t)))))

(define Ramanujuan
  (let ((s (pairs integers integers)))
    (define (iter last rest cnt)
      (let ((next (weight (stream-car rest))))
        (cond ((= next last) (iter next (stream-cdr rest) (inc cnt)))
              ((< cnt 2) (iter next (stream-cdr rest) 1))
              (else (cons-stream last
                                 (iter next (stream-cdr rest) 1))))))
    (iter (weight (stream-car s)) (stream-cdr s) 1)))

(display-stream-n Ramanujuan 6)
; 1729 4104 13832 20683 32832 39312 
