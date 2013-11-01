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

(define (cont-frac n d k)
  (define (term i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (term (+ i 1))))))
  (term 1))

; for the iterative, let's work backwards
(define (cont-frac-iter n d k)
  (define (term i result)
    (if (= i 0)
        result
        (term (- i 1) (/ (n i) (+ (d i) result)))))
  (term k 0))
        



(/ 1 (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 1))   ; 1 decimals of accuracy
(/ 1 (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 5))   ; 2 decimals
(/ 1 (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 10))  ; 4 decimals
(/ 1 (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 15))  ; 6 decimals
(/ 1 (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 20))  ; 8 decimals

(/ 1 (cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 1))   ; 1 decimals of accuracy
(/ 1 (cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 5))   ; 2 decimals
(/ 1 (cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 10))  ; 4 decimals
(/ 1 (cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 15))  ; 6 decimals
(/ 1 (cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 20))  ; 8 decimals





