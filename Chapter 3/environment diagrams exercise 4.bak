;(define (foo x) (- x))
(define foo
  (lambda (x)
    (- x)))

;(define (bar y) (foo (* y y)) )
(define bar
  (lambda (y)
    (foo (* y y))))

;(define (baz z)
;  (define (foo x) (+ x 1))
;  (set! foo bar)
;  (foo z))
(define baz
  (lambda (z)
    (define foo (lambda (x) (+ x 1)))
    (set! foo bar)
    (foo z)))

(baz 5)