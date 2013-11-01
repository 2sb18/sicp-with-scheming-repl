; kinda did it my way using linear transformations
(define (fib n)
  (define (loop a b c d n x1 x2)
    (cond ((= n 0) x1)
          ((even? n) (loop (+ (* a a) (* b c))
                           (+ (* a b) (* b d))
                           (+ (* a c) (* c d))
                           (+ (* c b) (* d d))
                           (/ n 2)
                           x1
                           x2))
          (else (loop a b c d (- n 1) (+ (* a x1) (* b x2)) (+ (* c x1) (* d x2))))))
  (loop 0 1 1 1 n 0 1))

; this is their way

(define (fibby n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))      ; compute p'
                   (+ (* q q) (* 2 q p))    ; compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))


