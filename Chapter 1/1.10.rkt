; Ackermann's function
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 10)
(A 2 4)
(A 3 3)

; this is currying!
; f(n) = 2*n : 0, 2, 4, 6, 8, ...
;   (A 0 n)
; = (* 2 n)
(define (f n) (A 0 n))
; g(n) = 2^n, except g(0) = 0 : 0, 2, 4, 8, 16, 32, 64, 32, etc
;   (A 1 n)
; = (* 2 (A 1 (- n 1)))
; = (* 2 (* 2 (A 1
(define (g n) (A 1 n))
;     0, 2, 4, 16, 65536
; h(n) = 2^h(n-1) where h(0) = 0
(define (h n) (A 2 n))
(define (k n) (* 5 n n))

