#lang planet neil/sicp


; for the iterative, let's work backwards
(define (cont-frac-iter n d k)
  (define (term i result)
    (newline)
    (display result)
    (if (= i 0)
        result
        (term (- i 1) (/ (n i) (+ (d i) result)))))
  (term k 0))

(define (tan-cf x k)
  (cont-frac-iter (lambda (i) (if (= i 1) (- 0 x) (- 0 (* x x))))
             (lambda (i) (- 1 (* 2 i)))
             k))

(tan-cf 2.0 10)
     
     
     


