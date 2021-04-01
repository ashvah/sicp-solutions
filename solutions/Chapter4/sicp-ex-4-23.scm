#lang sicp

; Alyssa
(lambda (env) (execute-sequence procs env))

; text
(lambda (env) ((lambda (env) ((lambda (env) ...) env) (procn-1 env)) env) (procn env))
