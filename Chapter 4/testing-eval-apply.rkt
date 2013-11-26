
#lang planet neil/sicp

(#%require "eval-apply.rkt")

;;;;;;;;;;;;;;;;;;;;;;;
; testing operations on environments
;;;;;;;;;;;;;;;;;;;;;;
; creating environment setup

(define environment-a (extend-environment '(a b c) '(5 6 7) the-empty-environment))
(define environment-b (extend-environment '(d e b) '(3 9 10) environment-a))
(define environment-c (extend-environment '() '() environment-b))
(add-binding-to-frame! 'd 0.1 (first-frame environment-c))

