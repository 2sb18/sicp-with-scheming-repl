(define (better-guess guess square)
  (* 0.5 (+ guess (/ square guess))))


(define (close-enough guess square)
  (if (> 0.00000001 (abs (- 1 (/ (* guess guess) square))))
      #t
      #f))

(define (newton-sqrt guess square)
  (if (close-enough guess square)
      guess
      (newton-sqrt (better-guess guess square) square)))




