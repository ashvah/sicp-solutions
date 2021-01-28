#lang sicp

; x=[a,b]
; y=[c,d]
; width of x is (b-a)/2
; width of y is (d-c)/2
; x+y=[a+c,b+d]
; x-y=[a-d,b-c]
; width of x+y is (b+d-a-c)/2=(b-a)/2+(d-c)/2 = (width of x) + (width of y)
; width of x-y is (b-c-a+d)/2=(b-a)/2+(d-c)/2 = (width of x) + (width of y)


; x=[1,2]
; y=[3,4]
; x*y=[3,8]
; x/y=[1/4,2/3]
; width of x is 1/2
; width of y is 1/2
; width of x*y is 5/2
; width of x/y is 5/24
