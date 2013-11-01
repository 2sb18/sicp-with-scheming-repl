#lang planet neil/sicp 

(#%require "streams.rkt")

;Exercise 3.65.  Use the series

; ln2 = 1 - 1/2 + 1/3 - 1/4 + ...
;to compute three sequences of approximations to the natural logarithm of 2, in the same way we did above for . How rapidly do these sequences converge? 


(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-summands (+ n 1)))))
(define ln2-stream
  (partial-sums (ln2-summands 1)))

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

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))


;(display-stream (accelerated-sequence euler-transform
;                                      -stream))

(display "unaccelerated\n")
(display-stream-partial ln2-stream 0 10)
(display "accelerated once\n")
(display-stream-partial (euler-transform ln2-stream) 0 10)
(display "accelerated twice\n")
(display-stream-partial (euler-transform (euler-transform ln2-stream)) 0 10)
(display "tableau sequence\n")
(display-stream-partial (accelerated-sequence euler-transform ln2-stream) 0 10)
; looking at the results:
; it's much faster to compute the approximation by using the euler-transform to make
; a tableau, looking at the first value of the stream of streams.
