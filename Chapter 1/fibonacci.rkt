
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))

(define (fibby n)
  (define (loop a b count)
    (if (= count n)
        a
        (loop b (+ a b) (+ count 1))))

  (loop 0 1 0))
  
  
