#lang sicp

(define (cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)
          (else (error "Undefined operation -- CONS" m))))
  dispatch)

(define (car z) (z 'car))
(define (cdr z) (z 'cdr))

(define (set-car! z new-value)
  ((z 'set-car!) new-value)
  z)

(define (set-cdr! z new-value)
  ((z 'set-cdr!) new-value)
  z)
 
; -->global: cons   car... cdr... set-car!... set-cdr!...
;             |  ^
;             v  |
;            (*)(*)
;          (v:x y)
;           (code)

(define x (cons 1 2))

; -->global: cons ...
;             x
;             |           ^
;             v           |
;            (*)(*)-->E1(x: 1)
;            (v:m)      (y: 2)  
;           (code)      (set-x!)
;             |         (set-y!)
;             +-------> (dispatch)

(define z (cons x x))

; -->global: cons ...
;             x                       z
;             |           ^           |             ^
;             v           |           v             |       
;            (*)(*)-->E1(x: 1)       (*)(*)--->E2(x: x)     
;            (v:m)      (y: 2)       (v:m)       (y: x)     
;           (code)      (set-x!)    (code)       (set-x!)   
;             |         (set-y!)      |          (set-y!)   
;             +-------> (dispatch)    +------->  (dispatch) 

; z->(*)(*)
;     |  |
; +---+--+
; |
; v
; x->(*)(*)
;     |  |
;     v  v
;     1  2

(set-car! (cdr z) 17)

; -->global: cons ...
;     ^       x                       z
;     |       |           ^           |             ^                   ^
;     |       v           |           v             |                   |
;     |      (*)(*)-->E1(x: 1)       (*)(*)--->E2(x: x)       -->E3(z: z)  (cdr z)
;     |      (v:m)      (y: 2)       (v:m)       (y: x)             
;     |     (code)      (set-x!)    (code)       (set-x!)     
;     |       |         (set-y!)      |          (set-y!)     -->E4(m: 'cdr) (z 'cdr)
;     |       +-------> (dispatch)    +------->  (dispatch)   <-------+
;     +------------+        ^
;                  |        +-----------------+
;    -->E5(z:x new-value:17) (set-car! x 17)  |
;                  +--------------------------+
;                  |                          |
;    -->E6(m: 'set-car!)  (x 'set-car!)       |
;                  +--------------------------+
;                  |
;    -->E7(v: 17) (set-x! 17)

; -->global: cons ...
;             x                       z
;             |           ^           |             ^
;             v           |           v             |       
;            (*)(*)-->E1(x: 17)      (*)(*)--->E2(x: x)      
;            (v:m)      (y: 2)       (v:m)       (y: x)            
;           (code)      (set-x!)    (code)       (set-x!)  
;             |         (set-y!)      |          (set-y!)    
;             +-------> (dispatch)    +------->  (dispatch)  

; z->(*)(*)
;     |  |
; +---+--+
; |
; v
; x->(*)(*)
;     |  |
;     v  v
;    17  2

(car x)

; -->global: cons ...
;             x                       z
;             |           ^           |             ^                  ^
;             v           |           v             |                  |
;            (*)(*)-->E1(x: 1)       (*)(*)--->E2(x: x)       -->E8(z: x)  (car x)
;            (v:m)      (y: 2)       (v:m)       (y: x)               
;           (code)      (set-x!)    (code)       (set-x!)      
;             |         (set-y!)      |          (set-y!)     -->E9(m: 'cdr) (x 'cdr)
;             +-------> (dispatch)    +------->  (dispatch)   <-------+
; 17
