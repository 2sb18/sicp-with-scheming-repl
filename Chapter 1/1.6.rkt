(define (better-guess guess square)
  (* 0.5 (+ guess (/ square guess))))


(define (close-enough guess square)
  (if (> 0.00000001 (abs (- 1 (/ (* guess guess) square))))
      #t
      #f))

(define (newton-sqrt guess square)
  (new-if (close-enough guess square)
      guess
      (newton-sqrt (better-guess guess square) square)))
      ; run into problems on this last line. This is evaluated, leading to a loop.
      ; if we used the (if) function, then else clause isn't looked at at all if 
      ; the predicate is false, leading to no loop

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))






