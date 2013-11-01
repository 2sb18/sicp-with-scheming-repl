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
          (display "enumerate ") (display start) (newline)
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

; integers could be defined as follows:

(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams (stream-cdr fibs)
                                         fibs))))
; fibs goes '(0 1 1 2 3 5 8 13 21 ...)
; stream-cdr fibs = '(1 1
; fibs            = '(0 1 1

(define s (cons-stream 1 (add-streams s s)))
; s = '(1 2 4 8 16 32 64 ...)
;       '(2 4 8

(define ones (cons-stream 1 ones))

