


#lang planet neil/sicp 

(#%require "streams.rkt")

; ; if we sent (integral '(1 2 3 4 ...) 0 0.5)
; (define (integral delayed-integrand initial-value dt)
;   (define int
;     (cons-stream initial-value
;                  (let ((integrand (force delayed-integrand)))
;                    (add-streams (scale-stream integrand dt)
;                                 int))))
;   int)


(define (integral delayed-integrand initial-value dt)
  (cons-stream initial-value
               (let ((integrand (force delayed-integrand)))
                 (if (stream-null? integrand)
                   the-empty-stream
                   (integral (delay (stream-cdr integrand))
                             (+ (* dt (stream-car integrand))
                                initial-value)
                             dt)))))

(define (solve-2nd a b y0 dy0 dt)
  (display "meow")
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay d2y) dy0 dt))
  (define d2y (add-streams (scale-stream dy a)
                           (scale-stream y  b)))
  y)

; ; we want to return the stream y
; ; let's assume y0 is 0 and dt is one to simplify
; ; y will start off with int = (0 (delay (add-streams dy int)))
; ; dy will start off ((f 
; (define (solve f y0 dt)
;   ; we don't know dy yet, but we could in principle start creating
;   ; y without knowing dy, so let's put a delay around dy, and then
;   ; change integral to expect a delayed signal
;   ; HAVE NO IDEA WHY I NEED THIS!
;   ;(display "meow")
;   (define y (integral (delay dy) y0 dt))
;   (define dy (stream-map f y))
;   y)

(display-stream-partial (solve-2nd 0 -1 2 3 0.001) 0 10000)


