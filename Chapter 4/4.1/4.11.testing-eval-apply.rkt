



#lang planet neil/sicp

(#%require "eval-apply.rkt")

;;;;;;;;;;;;;;;;;;;;;;;
; testing operations on environments
;;;;;;;;;;;;;;;;;;;;;;
; creating environment setup

; old way
; (define environment-a (extend-environment '(a b c) '(5 6 7) the-empty-environment))
; (define environment-b (extend-environment '(d e b) '(3 9 10) environment-a))
; (define environment-c (extend-environment '() '() environment-b))
; (add-binding-to-frame! 'd 0.1 (first-frame environment-c))

; ; the new 4.11 way!
(define a (empty-environment))
(define b (extend-environment '() a))
(define c (extend-environment '() b))

(add-binding-to-frame! 'a 5 a)
(add-binding-to-frame! 'b 6 a)
(add-binding-to-frame! 'c 7 a)

(add-binding-to-frame! 'd 3 b)
(add-binding-to-frame! 'e 9 b)
(add-binding-to-frame! 'b 10 b)

(add-binding-to-frame! 'd 0.1 c)

