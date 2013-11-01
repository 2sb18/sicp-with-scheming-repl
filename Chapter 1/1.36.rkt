#lang planet neil/sicp

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 0.00001))
  (define (test guess)
    (let ((output (f guess)))
      (newline)
      (display output)
      (if (close-enough? output guess)
          output
          (test output))))
  (test first-guess))

(fixed-point (lambda (x) (/ (log 1000) (log x))) 4.5) ; 23 steps


(fixed-point (lambda (x) (* 0.5 (+ x (/ (log 1000) (log x))))) 4.5)  ; 6 steps



