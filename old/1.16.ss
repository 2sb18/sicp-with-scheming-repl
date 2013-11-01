; Exercise 1.16
; design an iterative exponentiation process that uses successive squaring and uses a log number of steps
;
; in general, the technique of defining an invariant quantity that remains unchanged from state to state is a powerful way to think about the design of iterative algorithms.
(define (exp-meow b n)
  (exp-iter b n 1))

(define (exp-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (exp-iter (square b) (/ n 2) a))
        (else (exp-iter b (- n 1) (* a b)))))

(define (square x) (* x x))
