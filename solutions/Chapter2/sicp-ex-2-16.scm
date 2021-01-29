#lang sicp

; That's because we cannot tell whether two intervals with the same upper bound and lower bound is the same interval.
; For example
; a=[1,2], b=[1,2]
; (add-interval a b) the two arguments of add-interval are different
; (add-interval a a) the two arguments of add-interval are identical
; [1,2]=[1,2] is not necessarily true 
; so (a+b)c=ac+bc and other expressions may be false
; I'm not sure whether it's possible to overcomes these problems
; maybe we can do this if we can check the address of the arguments before passing it
