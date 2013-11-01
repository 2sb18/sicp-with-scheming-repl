(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

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





