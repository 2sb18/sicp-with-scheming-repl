
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

; the new 4.11 way!
(define environment-a (extend-environment (cons (cons 'a 5) (cons (cons 'b 6) (cons (cons 'c 7) '()))) the-empty-environment))
(define environment-b (extend-environment (cons (cons 'd 3) (cons (cons 'e 9) (cons (cons 'b 10) '()))) environment-a))
(define environment-c (extend-environment '() environment-b))

; this isn't working
; was it working before?
(add-binding-to-frame! 'd 0.1 (first-frame environment-c))

