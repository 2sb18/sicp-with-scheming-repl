(define (improve-sqrt guess square)
  (* 0.5 (+ guess (/ square guess))))

(define (improve-cbrt guess cube)
  (/ (+ (/ cube (* guess guess)) (* 2 guess))
     3))


(define (close-enough x y)
  (if (> 0.00000001 (abs (- 1 (/ x y))))
      #t
      #f))

(define (newton-sqrt guess square)
  (if (close-enough (* guess guess) square)
      guess
      (newton-sqrt (improve-sqrt guess square) square)))

(define (newton-cbrt guess cube)
  (if (close-enough (* guess guess guess) cube)
      guess
      (newton-cbrt (improve-cbrt guess cube) cube)))







