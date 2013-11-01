; Exercise 1.15
; estimating sin x
; p is applied 5 times with (sine 12.15)
; order of growth in time is O(n)= log n
; order of growth in space is O(n)= log n

(define (cube x) (* x x x))
(define (p x) ( - (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (< (abs angle) 0.1)
      angle
      (p (sine (/ angle 3.0)))))
