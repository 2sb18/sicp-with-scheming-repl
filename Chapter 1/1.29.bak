#lang planet neil/sicp

(define (crap-sum-integers a b)
  (if (> a b)
      0
      (+ a (crap-sum-integers (+ a 1) b))))

(define (square n)
  (* n n))

(define (cube n)
  (* n n n))

(define (crap-sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (crap-sum-cubes (+ a 1) b))))

(define (crap-pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (crap-pi-sum (+ a 4) b))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (identity n)
  n)
(define (inc n)
  (+ n 1))

(define (sum-integers a b)
  (sum identity a inc b))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (inc-4 n)
  (+ n 4))
(define (pi-term n)
  (/ 1.0 (* n (+ n 2))))

(define (pi-sum a b)
  (sum pi-term a inc-4 b))

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* dx (sum f (+ a (/ dx 2.0)) add-dx b)))