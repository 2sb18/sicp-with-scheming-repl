; exercise 1.17
(define (mult-recur a b)
  (cond ((= b 1) a)
        ((even? b) (mult-recur (* a 2) (/ b 2)))
        (else (+ a (mult-recur a (- b 1))))))