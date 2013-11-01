; recursive
(define (fr n)
  (cond ((< n 3) n)
        (else (+ (* 1 (fr (- n 1)))
                 (* 2 (fr (- n 2)))
                 (* 3 (fr (- n 3)))))))

; iterative
(define (fi n)
  (define (loop f f-1 f-2 f-3 i)
    (if (= i n) f
        (loop (+ f (* 2 f-1) (* 3 f-2))
              f
              f-1
              f-2
              (+ i 1))))
  (cond ((< n 3 ) n)
        (else (loop 2 1 0 0 2))))



