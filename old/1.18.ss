; exercise 1.18
;
; multiplying using double, halve, and addition. iterative procedure

(define (fast-mult a b)
  (fast-mult-iter 0 a b ))

(define (double x) (* x 2))
(define (halve x) (/ x 2))

(define (fast-mult-iter answer a b)
  (cond ((= b 0) answer)
        ((even? b) (fast-mult-iter answer (double a) (halve b)))
        (else (fast-mult-iter (+ answer a) a (- b 1)))))
  
