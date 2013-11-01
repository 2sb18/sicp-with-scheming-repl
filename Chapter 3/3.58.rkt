#lang planet neil/sicp


;Exercise 3.58.  Give an interpretation of the stream computed by the following procedure:

(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

;(Quotient is a primitive that returns the integer quotient of two integers.) What are the successive elements produced by (expand 1 7 10) ? What is produced by (expand 3 8 10) ? 

; (expand 1 7 10)
; 1 4 2 8 5 7 1 4 2

; (expand 3 8 10)
; 3 7 5 0 0 0 0 0 0....

; ANSWER: expand allows you to do a division where every element is a number in base radix

(define (stream-ref s n)
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))

(define (display-stream-partial stream start end)
  (display (stream-ref stream start))
  (newline)
  (if (not (eq? start end))
    (display-stream-partial stream (+ 1 start) end)))

(define (stream-car s)
  (car s))

(define (stream-cdr s)
;  (display "stream-cdr happening\n")
  (force (cdr s)))

(define one (expand 1 7 10))
(define two (expand 3 8 10))

(display-stream-partial one 0 10)
(display-stream-partial two 0 10)
