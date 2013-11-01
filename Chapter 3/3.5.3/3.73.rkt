

#lang planet neil/sicp 

(#%require "streams.rkt")

; if we sent (integral '(1 2 3 4 ...) 0 0.5)
(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value (add-streams (scale-stream integrand dt)
                                            int)))
  int)

(define (RC resistor-value capacitor-value dt)
  (define (RC-voltages current-stream initial-cap-voltage)
    (add-streams (scale-stream current-stream resistor-value)
                 (scale-stream (integral current-stream (* initial-cap-voltage capacitor-value) dt) (/ 1 capacitor-value))))
  RC-voltages)

(define RC1 (RC 5 1.0E-6 0.5))
(display "RC circuit with 5 ohms and 1uF cap, dt=0.5\n")
(display-stream-partial (RC1 (constant-stream 1.0E-3) 0) 0 20)
(newline)
(define RC2 (RC 1000.0 1.0E-6 1.0E-6))
(display "RC circuit with 1kohms and 1uF cap, dt=1.0E-6\n")
(define RC2-output (RC2 (constant-stream 1.0E-3) 0))
(display "voltage at 1 second: ") (display (stream-ref RC2-output 1.0E6))
(newline)
(display "should be 1000 volts!")
