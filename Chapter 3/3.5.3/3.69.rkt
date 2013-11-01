
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
  (display "interleaving\n")
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

;Exercise 3.69.  Write a procedure triples that takes three infinite streams, S, T, and U, and produces the stream of triples (Si,Tj,Uk) such that i < j < k. Use triples to generate the stream of all Pythagorean triples of positive integers, i.e., the triples (i,j,k) such that i < j and i^2 + j^2 = k^2. 

; going to do it my way, without the interleave, which I find easier to understand

(define (triples i j k)
  (if (>= i j)
    (triples 1 (+ 1 j) k)
    (if (>= j k)
      (triples 1 2 (+ 1 k))
      (cons-stream (list i j k) (triples (+ 1 i) j k)))))

(define pythagorean-triples 
  (stream-filter (lambda (triple)
                   (eq? (+ (square (car triple)) (square (cadr triple)))
                        (square (caddr triple))))
                 (triples 1 2 3)))

(display-stream-partial pythagorean-triples 0 200)
