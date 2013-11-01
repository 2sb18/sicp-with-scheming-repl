#lang planet neil/sicp

; implementation layer

(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

; user layer

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))


(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))


(define (div-interval x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (width-interval x)
  (/
   (- (upper-bound x) (lower-bound x))
   2))


(define z1 (make-interval 1 3))
(define z2 (make-interval 5 6))
(define z3 (make-interval 11 13))
(define z4 (make-interval 15 16))
(width-interval (add-interval z1 z2))
(width-interval (add-interval z3 z4))
(newline)
(width-interval (sub-interval z1 z2))
(width-interval (sub-interval z3 z4))
(newline)
(width-interval (mul-interval z1 z2))
(width-interval (mul-interval z3 z4))
(newline)
(width-interval (div-interval z1 z2))
(width-interval (div-interval z3 z4))

