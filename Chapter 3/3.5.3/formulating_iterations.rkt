#lang planet neil/sicp 

(#%require "streams.rkt")

(define (average a b)
  (/ (+ a b) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

; if n=1, this would give:
; 1, 1/3, 1/5, 1/7, etc
(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (partial-sums (scale-stream (pi-summands 1) 4)))

; this takes in a stream and returns an accelerated stream
(define (euler-transform s)
  (let ((s0 (stream-ref s 0))           ; Sn-1
        (s1 (stream-ref s 1))           ; Sn
        (s2 (stream-ref s 2)))          ; Sn+1
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

; this only gives us the first term of each series
(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

;(display-stream-partial (euler-transform pi-stream) 0 50)
(display-stream-partial (accelerated-sequence euler-transform pi-stream) 0 50)
