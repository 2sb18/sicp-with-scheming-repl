#|
(define (y)
  (let ((x #f))
    x))
|#

(define (y)
  ((lambda (x)
     x) #f))

(y)




#|
(define (test-func)
  (let ((a #f))
    (define (b input)
      (set! a input)
      a)
    b))

|#

; get rid of the 'let' syntactic sugar

(define test-func
  (lambda ()
    ((lambda (a)
       (define b
         (lambda (input)
           (set! a input)
           a))
       b) #f)))

(define x (test-func))
(x 3)

