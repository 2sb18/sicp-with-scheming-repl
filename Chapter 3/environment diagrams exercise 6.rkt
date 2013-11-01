



(define x 4)

(define baz
  (lambda (x)
    (define * (lambda (a b) (+ a b)))
    (lambda (y) (* x y))))

(define foo (baz (* 3 10)))

(foo (* 2 x))


; in E2, P3 is evaluated.
;  - come to the function call (* x y)
;  - new frame E3 is created which points to E1.
;  - in E3, a: 30 and b: 8. evaluated to be 38