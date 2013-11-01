(define (make-monitored funct)
  (let ((calls 0))
    (lambda (x)
      (if (eq? x 'how-many-calls?)
          calls
          (begin (set! calls (+ calls 1))
                 (funct x))))))

(define s (make-monitored sqrt))

(s 100)

(s 'how-many-calls?)