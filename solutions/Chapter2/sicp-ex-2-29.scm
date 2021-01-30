#lang sicp

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

; a)
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (car (cdr mobile)))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (car (cdr branch)))

(define structure-a 70)
(define structure-b 50)
(define structure-c 20)
(define branch-a (make-branch 5 structure-a))
branch-a
; (5 70)
(define branch-b (make-branch 7 structure-b))
branch-b
; (7 50)
(define mobile-a (make-mobile branch-a branch-b))
mobile-a
; ((5 70) (7 50))
(define branch-c (make-branch 3 structure-c))
branch-c
; (3 20)
(define branch-d (make-branch 8 mobile-a))
branch-d
; (8 ((5 70) (7 50)))
(define mobile-b (make-mobile branch-c branch-d))
mobile-b
; ((3 20) (8 ((5 70) (7 50))))
(left-branch mobile-b)
; (3 20)
(right-branch mobile-b)
; ((3 20) (8 ((5 70) (7 50))))
(branch-length branch-a)
; 5
(branch-structure branch-d)
; ((5 70) (7 50))

; b)
(define (get-branch-weight branch)
  (let ((s (branch-structure branch)))
    (if (pair? s)
        (total-weight s)
        s)))

(define (total-weight mobile)
  (let ((a (left-branch mobile))
        (b (right-branch mobile)))
    (+ (get-branch-weight a)
       (get-branch-weight b))))

(total-weight mobile-a)
; 120
(total-weight mobile-b)
; 140

; c)
(define (balance? mobile)
  (cond ((not(pair? mobile)) #t)
        (else (let ((a (left-branch mobile))
                    (b (right-branch mobile)))
                    (and (balance? (branch-structure a))
                         (balance? (branch-structure b))
                         (= (* (branch-length a) (get-branch-weight a))
                            (* (branch-length b) (get-branch-weight b))))))))

(balance? mobile-a)
; #t
(balance? mobile-b)
; #f

; d) 4
; I just need to define new left-branch, right-branch, branch-length and branch-structure
(define (make-mobile-new left right)
  (cons left right))

(define (make-branch-new length structure)
  (cons length structure))

(define (left-branch-new mobile)
  (car mobile))
(define (right-branch-new mobile)
  (cdr mobile))
(define (branch-length-new branch)
  (car branch))
(define (branch-structure-new branch)
  (car cdr branch))
