

#lang planet neil/sicp 

(#%require "streams.rkt")

; if we sent (integral '(1 2 3 4 ...) 0 0.5)
(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value (add-streams (scale-stream integrand dt)
                                            int)))
  int)

(display-stream-partial (integral (integers-starting-from 1) 0 0.5) 0 10)
