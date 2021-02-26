#lang sicp

; recurssive
; -> global: factorial  
;               |       ^
;               v       |
;         (variable: n)(*)
;            (code)
; -> global:
;        ^                    ^                            ^
;        |                    |           .....            |
; -> E1 (n: 6)          ->E2  (n: 5)                ->E6  (n: 1)
;(* 6 (factorial ...))  (* 5 (factorial ...))              1

; iterative
; -> global:   factorial             fact-iter
;               |     ^             |           ^
;               v     |             v           |
;       (variable: n)(*)  (variable: product   (*)
;          (code)                    counter
;                                    max-count)
;                                 (code)
; -> global:
;        ^                        ^                                        ^
;        |                        |               .....                    |
; -> E1 (n: 6)          ->E2  (product: 1                      ->E8  (product: 720
; (fact-iter 1 1 6)            counter: 1                             counter: 7
;                              max-count: 6)                          max-count 6)
;                             (fact-iter ...)                         720
