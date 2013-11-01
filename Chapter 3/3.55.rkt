#lang planet neil/sicp

(define (stream-for-each proc s)
  (if (stream-null? s)
    'done
    (begin (proc (stream-car s))
           (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))


(define (stream-ref s n)
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))

(define (stream-car s)
  (car s))

(define (stream-cdr s)
  (force (cdr s)))

(define (stream-enumerate-interval start end)
  (cond ((> start end)
         the-empty-stream)
        (else
          (cons-stream start (stream-enumerate-interval (+ start 1)
                                                            end)))))

(define (stream-map proc . streams)
  (if (stream-null? (car streams))
    the-empty-stream
    (cons-stream (apply proc (map stream-car streams))
                 (apply stream-map (cons proc (map stream-cdr streams))))))


; this takes in a stream and outputs a stream
(define (stream-filter predicate stream)
  (cond ((stream-null? stream)
         the-empty-stream)
        ((predicate (stream-car stream))
         (cons-stream (stream-car stream)
               (stream-filter predicate (stream-cdr stream))))
        (else (stream-filter predicate (stream-cdr stream)))))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define (divisible? x y) (= (remainder x y) 0))
; using the sieve of Eratosthenes to get the primes
(define (sieve stream)
   (cons-stream
     (stream-car stream)
     (sieve (stream-filter
              (lambda (x)
                (not (divisible? x (stream-car stream))))
              (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))

(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define factorials (cons-stream 1 (mul-streams factorials (integers-starting-from 2))))

;Exercise 3.55.  Define a procedure partial-sums that takes as argument a stream S and returns the stream whose elements are S0, S0 + S1, S0 + S1 + S2, .... For example, (partial-sums integers) should be the stream 1, 3, 6, 10, 15, .... 

; '(1 2 3 4) would turn into '(0 1 2 3 4)
(define (stream-delay s)
  (cons-stream 0
               s))

(define (partial-sums s) (add-streams s (cons-stream 0 (partial-sums s))))
