
; try doing this in C. It's painful!
(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))


; the mutex has a procedure called acquire and a procedure called release

; (make-serializer) creates an "object" and returns a procedure which we'll
; call the serializer (it is called by anything since it's an anonymous procedure)

; the serializer (a procedure) that is created looks like this:

(lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)

; where mutex is a local mutex object that belongs to the serializer.

; so serializer takes a procedure, then creates a new procedure which it returns.
; this new procedure takes in any number of arguments, then it aquires the serializers
; mutex, then runs the original procedure, releases the mutex, and returns the original
; procedure's return value.


(define (make-mutex)
  (let ((cell (list false)))      ; each mutex has a cell           
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))
(define (clear! cell)
  (set-car! cell false))

