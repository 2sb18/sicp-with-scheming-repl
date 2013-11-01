; since Scheme is applicative-order, p is going to be evaluated before
; sent to the function 'test', so it's going to run a loop

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

(test 0 (p))


