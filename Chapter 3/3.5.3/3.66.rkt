
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
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(display-stream-partial (pairs (integers-starting-from 0) (integers-starting-from 0)) 0 20)

;Exercise 3.66.  Examine the stream (pairs integers integers). Can you make any general comments about the order in which the pairs are placed into the stream? For example, about how many pairs precede the pair (1,100)? the pair (99,100)? the pair (100,100)? (If you can make precise mathematical statements here, all the better. But feel free to give more qualitative answers if you find yourself getting bogged down.) 

; MY ANSWER

; (x, x) happens at SUM from i=0 to i=x of 2^i
; which equals 2^(x+1) - 1
; so (0,0) happens at 1
; (3, 3) happens at 15
; so (100,100) happens at 2^(101) - 1 ~= 2.5E30

; (x,x+1) happens at 2^(x+1)+2^x-1 = 3 * 2^x - 1
; so (99, 100) happens at 3 * 2^99 ~= 1.90E30

; (x,y) where y is not less than x+2, happens at
; 3 * 2^x - 1 + (y-x-1) * 2^(x+1)
;
; so (1, 100) happens at:
; 3 * 2^1 - 1 + (100-1-1) * 2^2
; 6       - 1 + 98        * 4
; 397
; so there's 396 before (1,100)
