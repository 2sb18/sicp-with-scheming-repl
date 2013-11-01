(define (fast-expt b n)
  (define (square x) (* x x))
  (cond ((= n 0) 1)
        ; even
        ((even? n) (square (fast-expt b (/ n 2))))
        ; odd
        (else (* b (fast-expt b (- n 1))))))
