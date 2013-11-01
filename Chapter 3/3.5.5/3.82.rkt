
#lang planet neil/sicp 

(#%require "../streams.rkt")

(#%require (only racket/base random))

; this random function kinda l
(define (random-in-range low high)
  (+ low (* (random) (- high low))))

; experiment-stream is either 1 for pass or 0 for fail
; returns a stream of the fraction of successes
(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

; creates a 1 in the stream random number that is between x1 and x2
; creates a y that is between y1 and y2, otherwise, it
; creates a 0 in the stream. 
(define (estimate-integral-stream P x1 x2 y1 y2)
  (cons-stream (P (random-in-range x1 x2) (random-in-range y1 y2))
               (estimate-integral-stream P x1 x2 y1 y2)))


(define estimate-pi-integral
  (stream-map (lambda (p) (* 4.0 p))
              ; this gives a stream of increasing accuracy of
              ; whether two random numbers are relatively prime.
              (monte-carlo (estimate-integral-stream (lambda (x y) (>= 1 (+ (* x x)
                                                                            (* y y)))) 
                                                     -1 1 -1 1) 0 0)))

(stream-ref estimate-pi-integral 100000)
