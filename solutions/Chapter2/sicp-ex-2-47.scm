#lang sicp

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame1 frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame1 frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame1 frame))))))

(define (make-vect x y)
  (list x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (car (cdr v)))

(define (add-vect v1 v2)
  (map + v1 v2))

(define (sub-vect v1 v2)
  (map - v1 v2))

(define (scale-vect s v)
  (map (lambda (x) (* x s)) v))

(define (make-frame1 origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame1 f)
  (car f))

(define (edge1-frame1 f)
  (car (cdr f)))

(define (edge2-frame1 f)
  (car (cdr (cdr f))))

(define (make-frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame2 f)
  (car f))

(define (edge1-frame2 f)
  (car (cdr f)))

(define (edge2-frame2 f)
  (cdr (cdr f)))


(define f1 (make-frame1 (make-vect 0 0) (make-vect 1 2) (make-vect 3 4)))
(origin-frame1 f1)
; (0 0)
(edge1-frame1 f1)
; (1 2)
(edge2-frame1 f1)
; (3 4)
(define f2 (make-frame2 (make-vect 0 0) (make-vect 1 2) (make-vect 3 4)))
(origin-frame2 f2)
; (0 0)
(edge1-frame2 f2)
; (1 2)
(edge2-frame2 f2)
; (3 4)

((frame-coord-map f1) (make-vect 1 1))
; (4 6)
