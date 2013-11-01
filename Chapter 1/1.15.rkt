; figured out the inequality 12.15/3^2 < 0.1
; (p x) has to be called 5 times. let's test.

; order of growth in steps = log a
; order of growth in space = log a

(define t 0)

(define (cube x) (* x x x))
(define (p x)
  (set! t (+ t 1))
  (- (* 3 x) (* 4 (cube x))))



(define (sine angle)
  (set! t 0)
  (if (not (> (abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0)))))