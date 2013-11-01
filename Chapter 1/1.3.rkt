; takes three numbers, returns the sum of squares of the larger two
(define (summies a b c)
  (cond ((and (<= a b) (<= a c)) (+ (* b b) (* c c)))
        ((and (<= b a) (<= b c)) (+ (* a a) (* c c)))
        (else (+ (* a a) (* b b)))))
