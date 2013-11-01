#lang planet neil/sicp


;(define (memo-proc proc)
;  (let ((already-run? false) (result false))
;    (lambda ()
;      (if (not already-run?)
;          (begin (set! result (proc))
;                 (set! already-run? true)
;                 result)
;          result))))
; (delay <exp>) = (memo-proc (lambda () <exp>))

(define (add n1 n2)
  (display (+ n1 n2))
  (newline)
  (+ n1 n2))

(* (force (delay (add 1 2))) (* (force (delay (add 5 6)))))
(newline)
(* (force (delay (add 1 2))) (* (force (delay (add 1 2)))))

; let's say we want to run
; (delay (+ 1 3))
; this is going to run...
; (memo-proc (lambda () (delay (+ 1 3))))
; in memo-proc, 
(define (memo-proc proc) ; proc = (lambda () (delay (+ 1 3)))
  (let ((already-run? false) (result false))
    (lambda ()       ; going to return a procedure
      (if (not already-run?)   ; this is the first time it's run
          (begin (set! result (proc))  ; result is recorded
                 (set! already-run? true)
                 result)
          result))))           ; next time it's run, result is just given.

; but what if we had to calculate (+ 1 3) in two places in the program?
; we'd have do the calculation twice

