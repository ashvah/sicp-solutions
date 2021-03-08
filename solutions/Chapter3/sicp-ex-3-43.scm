#lang sicp

; current state: 10,20,30
; 1) 20,10,30
; 2) 30,20,10
; 3) 10,30,20
; after odering, it's still 10,20,30
; the state won't change

; 10     20       30
;    20-10   30-20
; 20     10    
;        20       20
