#lang racket

(define x (cons 1 2))
(define y (cons 4 3))
(define z (cons 1 4))

(define 1st (list x y z))

(define 2nd (take 1st 2))

(eq? (car 1st) (car 2nd))


