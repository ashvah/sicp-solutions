#lang sicp

(define a (make-connector))
(define b (make-connector))
(set-value! a 10 'user)

;--> global:a, set-value!, for-each-except, inform-about-value ...                    ^
;   ^       |                     |
;   |      (*)(*)---->E1(value:false  informant:false  constraints '())<-----E4(set-my-value, setter: 'user, newval: 10)
;   |       |             set-my-value, forget-my-value, connect
;   |       +-----------> me                            
;   |                                                   
;   +---------------------------+<----------E3(request: 'set-value!)
;                               |
;          -->E2(connector:a  new-value:10 informant:'user)

;--> global:a, set-value!, for-each-except, ...                    ^
;   ^       |                     |
;   |      (*)(*)---->E1(value:10  informant:'user  constraints '())
;   |       |             set-my-value, forget-my-value, connect
;   |       +-----------> me                            
;   |                                                   
;   +---------------------------+
;                               |
;          -->E5(exception:'user procedure inform-about-value constraints:'())
