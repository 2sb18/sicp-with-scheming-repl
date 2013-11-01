(define (double x) (* 2 x))
(define (halve x) (/ x 2))

(define (fast-mult-iter a b)
  (define (loop result a b)
    (cond ((= b 0) result)
          ((even? b) (loop result (double a) (halve b)))
          (else (loop (+ result a) a (- b 1)))))
  (loop 0 a b))