(define (factorial-iterate x)
  (define (fact-iter result y)
    (if (> y x)
        result
        (fact-iter (* result y) (+ y 1))))
  (fact-iter 1 1))

(define (factorial-recursive x)
  (if (= x 1)
      1
      (* x (factorial-recursive (- x 1)))))


