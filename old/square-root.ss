(define (square x)
  (* x x))
(define delta 0.0001)
(define (good-enough? guess x)
  (< (abs (- x (square guess))) delta))
(define (sqrt-itr guess previous-guess x)
  (if (good-enough-change? guess previous-guess)
      guess
      (sqrt-itr (improve guess x) guess x)))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (square-root x)
  (sqrt-itr 1.0 0.0 x))
(define (good-enough-change? current-guess previous-guess)
  (if (= current-guess 0)
      #f
      (< (/ (abs (- current-guess previous-guess)) current-guess) delta)))


