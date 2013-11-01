

(define (newton-sqrt square)
  (define (improve-sqrt guess)
    (* 0.5 (+ guess (/ square guess))))
  (define (close-enough x y)
    (if (> 0.00000001 (abs (- 1 (/ x y))))
        #t
        #f))
  (define (loop guess square)
    (if (close-enough (* guess guess) square)
        guess
        (loop (improve-sqrt guess) square)))
  (loop 1 square))
  







