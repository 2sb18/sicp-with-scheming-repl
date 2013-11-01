#lang planet neil/sicp 

(#%require "streams.rkt")

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

(define (average a b)
  (display (/ (+ a b) 2))
  (newline)
  (/ (+ a b) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (stream-limit stream tolerance) 
  (if (> tolerance (abs (- (stream-car stream) (stream-car (stream-cdr stream)))))
    (stream-car (stream-cdr stream))
    (stream-limit (stream-cdr stream) tolerance)))


; Exercise 3.64.  Write a procedure stream-limit that takes as arguments a stream and a number (the tolerance). It should examine the stream until it finds two successive elements that differ in absolute value by less than the tolerance, and return the second of the two elements. Using this, we could compute square roots up to a given tolerance by

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(display (sqrt 2 0.0000001))
