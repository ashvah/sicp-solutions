#lang sicp

(define (square x) (* x x))

(define (square-list1 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) 
              (cons (square (car things))
                    answer))))
  (iter items nil))

(define (square-list2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

(square-list1 (list 1 2 3 4 5))
; (25 16 9 4 1)
(square-list2 (list 1 2 3 4 5))
; (((((() . 1) . 4) . 9) . 16) . 25)

; That's because (cons elementa listb) is a list
; but (cons lista elementb) is not a list
; no matter whether we implement square-list in a recursive way or a iterative way
; there are only one order that allows us to form a list
; unfortunately they are in reverse order...
; in fact, exercise 2.18 also reflects this
