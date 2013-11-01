(define (make-object)
  (lambda (proc)
    (if (eq? proc 'meow)
        7
        6)))

(define x (make-object))
(x 'meow)
(x 'woof)