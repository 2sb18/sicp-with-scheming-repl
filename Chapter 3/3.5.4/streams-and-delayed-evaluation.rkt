
#lang planet neil/sicp 

(#%require "streams.rkt")

; if we sent (integral '(1 2 3 4 ...) 0 0.5)
(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt)
                                int))))
  int)

; we want to return the stream y
; let's assume y0 is 0 and dt is one to simplify
; y will start off with int = (0 (delay (add-streams dy int)))
; dy will start off ((f 
(define (solve f y0 dt)
  ; we don't know dy yet, but we could in principle start creating
  ; y without knowing dy, so let's put a delay around dy, and then
  ; change integral to expect a delayed signal
  ; HAVE NO IDEA WHY I NEED THIS!
  (display "meow")
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(stream-ref (solve (lambda (y) y) 1 0.001) 1000)


