(define (count-pairs x)
  (define (contains list-of-cons target)
    (cond ((eq? list-of-cons '()) #f)
          ((eq? (car list-of-cons) target) #t)
          (else (contains (cdr list-of-cons) target))))
  (let ((already-counted '()))
    (define (loop x)
      (if (or (not (pair? x)) (contains already-counted x))
          0
          (begin
            (set! already-counted (cons x already-counted))
            (+ (loop (car x))
               (loop (cdr x))
               1))))
      (loop x)))

; this should count three
(count-pairs '(a b c))

(define four
  (let ((c (cons 'b '())))
    (let ((b (cons c c)))
      (let ((a (cons 'a b)))
        a))))
(count-pairs four)

(define seven
  (let ((c (cons 'b '())))
    (let ((b (cons c c)))
      (let ((a (cons b b)))
        a))))
(count-pairs seven)

(define no-return
  (let ((c (cons 'b '())))
    (let ((b (cons c c)))
      (let ((a (cons b b)))
        (set-car! c a)
        a))))
(count-pairs no-return)
