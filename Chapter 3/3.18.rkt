(define (contains list-of-cons target)
  (cond ((eq? list-of-cons '()) #f)
        ((eq? (car list-of-cons) target) #t)
        (else (contains (cdr list-of-cons) target))))

(define (count-pairs x)
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

(define (contains-cycle? x)
  (let ((already-counted '()))
    (define (loop x)
      (cond ((null? x) #f)
            ((not (pair? x)) (error "input isn't a list -- CONTAINS-CYCLES?" x))
            ((contains already-counted x) #t)
            (else (begin
                    (set! already-counted (cons x already-counted))
                    (loop (cdr x))))))
    (loop x)))

(contains-cycle? '())


(define four
  (let ((c (cons 'b '())))
    (let ((b (cons c c)))
      (let ((a (cons 'a b)))
        a))))
(contains-cycle? four)

(define no-return
  (let ((c (cons 'b '())))
    (let ((b (cons c c)))
      (let ((a (cons b b)))
        (set-cdr! c a)
        a))))
(contains-cycle? no-return)
