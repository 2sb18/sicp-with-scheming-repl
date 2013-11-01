(define foo
  (lambda (x)
    (* x x)))

(define bar
  (lambda (x)
    (* x x)))

(define baz foo)

(define qux
  (lambda (x)
    foo))

(define zot
  (lambda (x)
    (foo x)))

((lambda (foo bar)
   (zot (+ foo bar)))
 3 5)

- come to ((lambda (foo bar) (zot (+ foo bar))) 3 5)
  - it's a procedural call, so we're going to have to create a frame
  - frame points to same frame that the procedure points to 