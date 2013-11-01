#lang planet neil/sicp

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))  

; why does the O go from O(log n) to O(n)?
; well, let's say we did 2^4. It's a tree. If we go
; to 2^8, we have to double that to two trees of 2^4 right
; under the 2^8. So we double n, but we double the work, 
; so O(n) 
(define (shit-expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (shit-expmod base (/ exp 2) m)
                       (shit-expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (shit-expmod base (- exp 1) m))
                    m))))