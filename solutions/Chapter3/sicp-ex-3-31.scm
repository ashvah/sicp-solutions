#lang sicp

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(probe 'sum sum)

(probe 'carry carry)


(half-adder input-1 input-2 sum carry)

; input-1 input-2 sum carry d e is initialized to 0
; but e should be 1

(set-signal! input-1 1)
(propagate)

; before (propagate) (input-1,input-2,d,carry,e,sum)=(0 0 0 0 0 0)
; input-1=1-> d=1, carry=0->sum=0 the answer is wrong
; because carry=0=old value, so the value of e won't change 
; e=0, a wrong value
; sum=0 also won't change.

(set-signal! input-2 1)
(propagate)

; before (propagate) (input-1,input-2,d,carry,e,sum)=(1 0 1 0 0 0)
; input-2=1-> d=1, carry=1->e=0
; carry changed so print the value of carry
; d changed but sum=0 so the info won't be printed
; carry 11  New-value = 1

; so we should initialize all wires to a proper signal
