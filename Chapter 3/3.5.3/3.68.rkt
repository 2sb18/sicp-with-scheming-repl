
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

; (define (pairs s t)
;   (cons-stream
;    (list (stream-car s) (stream-car t))
;    (interleave
;     (stream-map (lambda (x) (list (stream-car s) x))
;                 (stream-cdr t))
;     (pairs (stream-cdr s) (stream-cdr t)))))

;Exercise 3.68.  Louis Reasoner thinks that building a stream of pairs from three parts is unnecessarily complicated. Instead of separating the pair (S0,T0) from the rest of the pairs in the first row, he proposes to work with the whole first row, as follows:

; say s is 0123456789 and t is abcdefghijk

(define (pairs s t)
  (interleave
    (stream-map (lambda (x) (list (stream-car s) x)) 
                t)
    (pairs (stream-cdr s) (stream-cdr t))))

(display-stream-partial (pairs (integers-starting-from 10) (integers-starting-from 20)) 0 0)

;Does this work? Consider what happens if we evaluate (pairs integers integers) using Louis's definition of pairs. 

; MY ANSWER:

; The problem is that the pairs procedure has to do two things before it calls interleave procedure
; First it does that stream-map, which it can do, then it calls pairs.
; So we have an infinite amount of the pair procedure calling itself before interleave ever being called!
