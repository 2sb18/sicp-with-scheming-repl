(define (square x) (* x x))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ; even
        ((even? n) (square (fast-expt b (/ n 2))))
        ; odd
        (else (* b (fast-expt b (- n 1))))))

(define (fast-expt-iter b n)
  (define (loop a b n)
    (cond ((= n 0) a)
          ((even? n) (loop a (square b) (/ n 2)))
          (else (loop (* a b) b (- n 1)))))
  (loop 1 b n))