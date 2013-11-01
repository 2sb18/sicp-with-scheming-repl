#lang planet neil/sicp

; streams are going to have the same procedures as lists.
;
; lists have the following: cons, car, cdr, list-ref, map, for-each
; list-ref returns an item from a list. so (list-ref 3) would return
; the 3rd item from the list

(define (stream-ref s n)
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
    the-empty-stream
    (cons-stream (proc (stream-car s))
                 (stream-map proc (stream-cdr s)))))


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

(define (stream-car s)
  (car s))

(define (stream-cdr s)
  (force (cdr s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; in a normal list-enumerate, we'd just create a list
; from start to end. but with a stream, we just create
; the first number and promise to create the rest I guess?
(define (stream-enumerate-interval start end)
  (cond ((> start end)
         the-empty-stream)
        (else (cons-stream start (stream-enumerate-interval (+ start 1)
                                                            end)))))

; this takes in a stream and outputs a stream
(define (stream-filter predicate stream)
  (cond ((stream-null? stream)
         the-empty-stream)
        ((predicate (stream-car stream))
         (cons-stream (stream-car stream)
               (stream-filter predicate (stream-cdr stream))))
        (else (stream-filter predicate (stream-cdr stream)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; STREAM IN ACTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (enumerate-interval start end)
  (cond ((> start end)
         nil)
        (else (cons start (enumerate-interval (+ start 1) end)))))
(define (filter predicate lst)
  (cond ((null? lst) nil)
        ((predicate (car lst))
         (cons (car lst)
               (filter predicate (cdr lst))))
        (else (filter predicate (cdr lst)))))
(define (square x) (* x x))
(define (smallest-divisor n)
  (find-divisor n 2)) 
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))

(stream-car
  (stream-cdr
    (stream-filter prime?
                   (stream-enumerate-interval 10000 1000000))))

;(car (cdr (filter prime? (enumerate-interval 10000 1000000))))

; why does the list way take so long? 
; first we have to create a whole list from 10000 to one million
; then we have to filter through the whole thing
; why do we take the cdr of the filtered mess? cause the question
; asked for the 2nd prime


(define (stream-mapper proc . argstreams)
  (if (stream-null? (car argstreams))
    the-empty-stream
    (cons-stream
      (apply proc (map stream-car argstreams))
      (apply stream-mapper
             (cons proc (map stream-cdr argstreams))))))

;(define (stream-map proc s)
;  (if (stream-null? s)
;    the-empty-stream
;    (cons-stream (proc (stream-car s))
;                 (stream-map proc (stream-cdr s)))) 
;

(define a (stream-enumerate-interval 1 10))
(define b (stream-enumerate-interval 10 20))

(stream-car
  (stream-filter (lambda (x) (> x 100))
                 (stream-mapper * a b)))

                
