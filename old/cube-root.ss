(define (square x)
  (* x x))
(define (cube x)
  (* x x x))
(define delta 0.0001)
(define (average x y)
  (/ (+ x y) 2))
(define (cube-root x)
  (define (good-enough-change? current-guess previous-guess)
    (if (= current-guess 0) 
        #f
        (< (/ (abs (- current-guess previous-guess)) current-guess) delta)))
  (define (improve guess)
    (/ (+ (/ x (square guess)) (* 2 guess))
       3))
  (define (cube-root-itr guess previous-guess)
    (if (good-enough-change? guess previous-guess)
        guess
        (cube-root-itr (improve guess) guess)))
  (cube-root-itr 1.0 0.0))






