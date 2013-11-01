

#lang planet neil/sicp 

(#%require "streams.rkt")

(define (sign-change-detector current-value last-value)
  (cond ((and (>= current-value 0) (>= last-value 0)) 0)
        ((and (>= current-value 0) (<  last-value 0)) 1)
        ((and (<  current-value 0) (>= last-value 0)) -1)
        ((and (<  current-value 0) (<  last-value 0)) 0)
        (else (error "error -- SIGN-CHANGE-DETECTOR" current-value last-value))))

; before taking the zero-crossing, average each value of the sense data
; with the previous value
(define (make-zero-crossings input-stream last-average last-input)
  ; take the average of the first value of the input-stream
  ; and the last value
  (let ((avpt (/ (+ (stream-car input-stream) last-input) 2)))
    (cons-stream (sign-change-detector avpt last-average)
                 (make-zero-crossings (stream-cdr input-stream)
                                      avpt
                                      (stream-car input-stream)))))

; say we call (make-zero-crossings '(-100 3 5 -1 0 6) 0)
; avpt will be -100+0/2 = -50. (sign-change-detector -50 0) = 1
; now (make-zero-crossings '(3 5 -1 0 6 0) -50) is called.

; what we have to do is send the last input and the last-average value
; to make-zero-crossings. Need the last-average for the sign-change-detector,
; need the last-input to make the average. Implemented this.


(define sense-data (list-to-stream '(-100 3 5 -1 0 6)))
(define zero-crossings (make-zero-crossings sense-data 0 0))
(display-stream-partial zero-crossings 0 5)

