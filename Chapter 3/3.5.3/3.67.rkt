
#lang planet neil/sicp 

(#%require "streams.rkt")

; produce a stream of pairs of all integers (i,j) with
; i<=j such that i+j is prime

; should produce stuff like (2,1), (3,1), (3,2), (4,1), (4,2), (4,3)

(define (int-pair-stream j i)
  (if (>= i j)
    (int-pair-stream (+ j 1) 1)
    (cons-stream (cons j i) (int-pair-stream j (+ i 1)))))

(define int-pairs (int-pair-stream 2 1))

(define prime-sum-pairs (stream-filter (lambda (triplet)
                                         (prime? (caddr triplet))) 
                                       (stream-map (lambda (pair)
                                                     (list (car pair)
                                                           (cdr pair)
                                                           (+ (car pair) (cdr pair))))
                                                   int-pairs)))

; first take from s1, then from s2, then again from s1, then from s2,
; and so on.
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (interleave
        (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
        (stream-map (lambda (x) (list x (stream-car t))) (stream-cdr s)))
      (pairs (stream-cdr s) (stream-cdr t)))))

(display-stream-partial (pairs (integers-starting-from 0) (integers-starting-from 0)) 0 20)

;Exercise 3.67.  Modify the pairs procedure so that (pairs integers integers) will produce the stream of all pairs of integers (i,j) (without the condition i < j). Hint: You will need to mix in an additional stream. 
