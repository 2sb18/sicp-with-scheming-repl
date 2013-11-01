
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

(define (pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t) weight) weight)))

; s1 and s2 are streams of pairs
; s1 and s2 have to be in order according to weight already
(define (merge s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
          (let ((s1weight (weight (stream-car s1)))
                (s2weight (weight (stream-car s2))))
            (cond ((< s1weight s2weight)
                   (cons-stream (stream-car s1) (merge (stream-cdr s1) s2 weight)))
                  ((> s1weight s2weight)
                   (cons-stream (stream-car s2) (merge s1 (stream-cdr s2) weight)))
                  (else
                    (cons-stream (stream-car s1) (cons-stream (stream-car s2)
                                 (merge (stream-cdr s1)
                                        (stream-cdr s2) weight)))))))))

(define (sum pair)
  (+ (car pair) (cadr pair)))

(define a (pairs (integers-starting-from 1) (integers-starting-from 1)
                 sum))

;(display-stream-partial a 0 10)

(define b (stream-filter (lambda (pair)
                           (and (not (divides? 2 (car pair)))
                                (not (divides? 3 (car pair)))
                                (not (divides? 5 (car pair)))
                                (not (divides? 2 (cadr pair)))
                                (not (divides? 3 (cadr pair)))
                                (not (divides? 5 (cadr pair))))) 
                         (pairs (integers-starting-from 1) (integers-starting-from 1)
                                (lambda (pair)
                                  (+ (* 2 (car pair)) (* 3 (cadr pair)) (* 5 (car pair) (cadr pair)))))))

(display-stream-partial b 0 10)
