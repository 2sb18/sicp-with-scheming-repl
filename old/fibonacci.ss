(define (fibonacci n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fibonacci (- n 1)) (fibonacci (- n 2))))))

; computing fibo(8). 
(define (fibonacci-iterative n)
  (define (fibo-iter current previous index)
    (if (< index n) (fibo-iter (+ current previous) current (+ index 1))
        current))
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (fibo-iter 1 0 1))))
         

