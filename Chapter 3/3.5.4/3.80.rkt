


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

(define (RLC R L C dt)
  (define (RLC-circuit vc0 il0)
    (newline)
    (define vc (integral (delay dvc) vc0 dt))
    (define il (integral (delay dil) il0 dt))
    (define dvc (scale-stream il (/ -1 C)))
    (define dil (add-streams (scale-stream vc (/ 1 L))
                             (scale-stream il (/ (- 0 R) L))))
    (define vi-pair (stream-map cons vc il))
    vi-pair)
  RLC-circuit)

(define super-circuit! (RLC 1 1 0.2 0.00001))
(define super-stream! (super-circuit! 10 0 ))

; doing this simulation in LTspice, at 1 seconds, the voltage around the cap is -2.26V and the current through the inductor is 2.31A. With out simulation we go -2.34V and 2.29A. Pretty good!
; (display-stream-partial super-stream! 0 100000)
(stream-ref super-stream! 100000)
