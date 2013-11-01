#lang planet neil/sicp

(define (square x)
  (* x x))

; this procedure calls the g procedure with a value of 2
(define (f g)
  (g 2))

(f square)
(f (lambda (z) (* z (+ z 1))))

(f f)
; does this do the following?
; (f f)
; (f 2)
; (2 2)

; yes, it looks like it does!




