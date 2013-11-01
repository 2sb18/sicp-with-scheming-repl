#lang planet neil/sicp

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 0.00001))
  (define (test guess)
    (let ((output (f guess)))
      (if (close-enough? output guess)
          output
          (test output))))
  (test first-guess))

(fixed-point (lambda (x) (+ (* x -13/19 1.0) 13)) 0)
(fixed-point cos 1.0)
(fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)

(define (sqrt x)
  (fixed-point (lambda (y) (/ (+ (/ x y) y) 2)) 1.0))


