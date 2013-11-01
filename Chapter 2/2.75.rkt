#lang planet neil/sicp

(define (square n)
  (* n n))

; print list, can be nested
; makes all the spacing correct!
(define (pl n)
  (define (in-loop n space?)
    (cond ((null? n) 0)
          ((not (pair? n)) (if space? (display " ")) (display n))
          ((list? (car n)) (out-loop (car n) #t) (in-loop (cdr n) #t))
          (else (if space? (display " ")) (display (car n)) (in-loop (cdr n) #t)))) 
  (define (out-loop n space?)
    (cond ((list? n) (if space? (display " ")) (display "(") (in-loop n #f) (display ")"))
          (else (in-loop n #f))))
  (out-loop n #f) (display "\n"))

; in data-directed programming, the type of the data is sent along with the data.

; ie '(rectangular 3 4) or '(polar 3 4)

; a package is installed for each type, which has procedures on how to deal with that type.

; this means that each operation has to take care of its own dispatching.

; Here, (deriv) takes care of dispatching by getting the type of the exp.
; We call (deriv) an intelligent operation because it figures out what to call.
;(define (deriv exp var)
;  ((get 'deriv (operator exp)) (operands exp) var))



(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else
           (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)

; or if we have an apply-generic function like this...
(define (apply-generic op arg) (arg op))

(define (real-part z)
  (apply-generic 'real-part z))
         
         

; now if we did something like
(define z (make-from-real-imag 3 4))
; z would be a procedure that had 3 and 4 as x and y.

; then we could do this
(z 'real-part)  ; to get the real part

(define z2 (make-from-mag-ang 3 1))
(z2 'real-part)