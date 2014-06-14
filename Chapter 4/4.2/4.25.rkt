; Exercise 4.25.  Suppose that (in ordinary applicative-order Scheme) we define unless as shown above and then define factorial in terms of unless as

(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))

(define (factorial n)
  (unless (= n 1)
    (* n (factorial (- n 1)))
    1))

; What happens if we attempt to evaluate (factorial 5)? Will our definitions work in a normal-order language?

; lets just try 2
(factorial 2)
= (unless (= 2 1) (* 2 (factorial (- 2 1))) 1)
= (unless false (* 2 (factorial 1)) 1)
= (unless false (* 2 (unless (= 1 1) (* 1 (factorial (- 1 1))) 1)) 1)
= (unless false (* 2 (unless true (* 1 (factorial 0)) 1)) 1)
= (unless false (* 2 (unless true (* 1 (unless (= 0 1) (* 0 (factorial......)))))))

; no, "unless" won't work with applicative-order Scheme

; what about with normal-order Scheme?

(factorial 2)
= (unless (= 2 1) (* 2 (factorial (- 2 1))) 1)
= (if (= 2 1) 1 (* 2 (factorial (- 2 1))))
; if is a special type where predicate is evaluated before the other stuff
= (if false 1 (* 2 (factorial (- 2 1))))
= (* 2 (factorial (- 2 1)))
= (* 2 (unless (= (- 2 1) 1) (* (- 2 1) (factorial (- (- 2 1)))) 1))
= (* 2 (if (= (- 2 1) 1) 1 (* (- 2 1) (factorial (- (- 2 1))))))
= (* 2 (if true 1 yada-yada))
= (* 2 1)
= 2

; works in normal-order Scheme



