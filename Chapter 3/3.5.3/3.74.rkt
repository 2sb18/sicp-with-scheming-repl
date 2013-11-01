

#lang planet neil/sicp 

(#%require "streams.rkt")

(define (sign-change-detector current-value last-value)
  (cond ((and (>= current-value 0) (>= last-value 0)) 0)
        ((and (>= current-value 0) (<  last-value 0)) 1)
        ((and (<  current-value 0) (>= last-value 0)) -1)
        ((and (<  current-value 0) (<  last-value 0)) 0)
        (else (error "error -- SIGN-CHANGE-DETECTOR" current-value last-value))))


(define (make-zero-crossings input-stream last-value)
  (cons-stream
   (sign-change-detector (stream-car input-stream) last-value)
   (make-zero-crossings (stream-cdr input-stream)
                        (stream-car input-stream))))

(define sense-data (list-to-stream '(-100 3 5 -1 0 6)))
(define zero-crossings (make-zero-crossings sense-data 0))

(display "old way\n")
(display-stream-partial zero-crossings 0 5)

(define zero-crossings-new
  (stream-map sign-change-detector sense-data (cons-stream 0 sense-data)))

(display "new way\n")
(display-stream-partial zero-crossings-new 0 5)



