#lang planet neil/sicp
(#%require "eval-apply.rkt")

(#%provide
 run-and-check
 its-and-check
 )
; this kinda sucks cause we have to send pretty much the exact same thing twice
; HOW THE FUCK DO WE EVAL!!!!
(define (run-and-check quoted-thing-to-run thing-to-run expected-output)
  (display "Running: ") (display quoted-thing-to-run) (newline)
  (if (not (equal? expected-output thing-to-run))
    (error "ERROR:\n expected " expected-output "\nactual result: " thing-to-run))
  (plp thing-to-run)
  (newline)
  (newline))

(define (its-and-check thing-to-run expected-output)
  (display "ITS Running: ") (display thing-to-run) (newline)
  (let ((actual-output (its thing-to-run)))
    (if (not (equal? expected-output actual-output))
      (error "ERROR:\n expected " expected-output "\nactual result: " actual-output))
    (plp actual-output)
    (newline)
    (newline)))

