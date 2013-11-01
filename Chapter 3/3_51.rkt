#lang planet neil/sicp

(define (stream-ref s n)
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
    the-empty-stream
    (cons-stream (proc (stream-car s))
                 (stream-map proc (stream-cdr s)))))

(define (display-line x)
  (newline)
  (display x))

(define (stream-car s)
  (car s))

(define (stream-cdr s)
  (force (cdr s)))

(define (stream-enumerate-interval start end)
  (cond ((> start end)
         the-empty-stream)
        (else (cons-stream start (stream-enumerate-interval (+ start 1)
                                                            end)))))

;Exercise 3.51.  In order to take a closer look at delayed evaluation, we will use the following procedure, which simply returns its argument after printing it:

(define (show x)
  (display-line x)
  x)

;What does the interpreter print in response to evaluating each expression in the following sequence?59

(define x (stream-map show (stream-enumerate-interval 0 10)))
;(stream-ref x 5)
; what is x?
; x = (stream-map show (cons 0 (delay (stream-enumerate-interval 1 10))))
; x = (cons (show 0) (delay (stream-map show (stream-enumerate-interval 1 10))))
; x = (cons (show 0) (delay (stream-map show (cons 1 (delay (stream-enumerate-interval 2 10))))))
; x = (cons (show 0) (delay (cons (show 1 
; right here we're going to get a showing of the (show 0)
; x = (cons 0 (delay (stream-map show (cons 1 (delay (stream-enumerate-interval 2 10))))))
; now we call (stream-ref x 5)
; returns (stream-ref (stream-map show (cons 1 (delay (stream-enumerate-interval 2 10)))) 4)
;       = (stream-ref (cons (show 1) (delay (stream-map show (stream-enumerate-interval 2 10)))) 4)
; going to print 1 2 3 4 5


;(stream-ref x 7)
; going to print 6 7
