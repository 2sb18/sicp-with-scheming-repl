; Exercise 4.18.  Consider an alternative strategy for scanning out definitions that translates the example in the text to
;
(lambda <vars>
  (let ((u '*unassigned*)
        (v '*unassigned*))
    (let ((a <e1>)
          (b <e2>))
      (set! u a)
      (set! v b))
    <e3>))
;
; Here a and b are meant to represent new variable names, created by the interpreter, that do not appear in the user's program. Consider the solve procedure from section 3.5.4: (this solves the equation dy/dt = f(y) )
;
(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)
;
; Will this procedure work if internal definitions are scanned out as shown in this exercise? What if they are scanned out as shown in the text? Explain. 

; from the text:"This procedure does not work, because in the first line of solve the call to integral requires that the input dy be defined, which does not happen until the second line of solve."

; for the equation f(y) = y, we get dy/dt = y. dy/dt = 1. so the answer would be y = 1.

; here's what the integral function looks like
(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt)
                                int))))
  int)


; Here's what the function will look like if scanned out with the textbook strategy

(define solve
  (lambda (f y0 dt)
    (let ((y '*unassigned*)
          (dy '*unassigned*))
      (set! y (integral (delay dy) y0 dt))
      (set! dy (stream-map f y))
      y)))

; can't see why this wouldn't work, it's basically doing the same thing as the unscanned version

; Here's what the function would look like with the alternative scanning

(define solve
  (lambda (f y0 dt)
    (let ((y '*unassigned*)
          (dy '*unassigned*))
      (let ((a (integral (delay dy) y0 dt))
            (b (stream-map f y)))
        (set! y a)
        (set! dy b))
      y)))

; Would this work?
; Let's write in comments beside it.

(define solve
  (lambda (f y0 dt)
    (let ((y '*unassigned*)                   
          (dy '*unassigned*))
      (let ((a (integral (delay dy) y0 dt))     ; I think the dy is alright here because it's delayed
            (b (stream-map f y)))               ; what is y here? it's not defined yet, so this is definitely a problem, since stream-map needs it.
        (set! y a)
        (set! dy b))
      y)))
