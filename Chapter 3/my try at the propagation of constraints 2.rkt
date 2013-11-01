(define wire-resets '())

(define (reset-wires)
  (define (iter reset-wires-left)
    (if (not (null? reset-wires-left))
        (begin ((car reset-wires-left))
               (iter (cdr reset-wires-left)))))
  (iter wire-resets))
  
(define (make-wire constant-value)
  (let ((connections '()) (changeable-value '()))
    (define (wire-value)
      (if (null? constant-value)
          changeable-value
          constant-value))
    (define (broadcast-change connections-left)
      (if (not (null? connections-left))
          (begin ((car connections-left) (wire-value))
                 (broadcast-change (cdr connections-left)))))
    (define (dispatch op)
      (cond ((eq? op 'set-constant) (lambda (v) 
                                      (set! constant-value v)
                                      (broadcast-change connections)))
            ((eq? op 'set-value) (lambda (v)
                                   (if (not (equal? v (wire-value)))
                                       (begin (set! changeable-value v)
                                              (broadcast-change connections)))))
            ((eq? op 'get-value) (lambda () (wire-value)))
            ((eq? op 'connect) (lambda (proc) (set! connections (cons proc connections))))
            (else (error "no operation by that name" op))))
    (set! wire-resets (cons (lambda () (set! changeable-value '())) wire-resets))
    dispatch))

(define (get-wire-value wire) ((wire 'get-value)))
(define (set-wire-value wire value) ((wire 'set-value) value))
(define (set-wire-constant wire value) ((wire 'set-constant) value))


(define (simple-three-terminal in1 in2 out operation inverse-op)
   (define (in1-changed wire-value)
    (cond ((and (null? (get-wire-value in2)) (null? (get-wire-value out))) #f)
          ((null? (get-wire-value out)) (set-wire-value out (operation wire-value (get-wire-value in2))))
          ((null? (get-wire-value in2)) (set-wire-value in2 (inverse-op (get-wire-value out) wire-value)))
          (else #f)))
  (define (in2-changed wire-value)
    (cond ((and (null? (get-wire-value in1)) (null? (get-wire-value out))) #f)
          ((null? (get-wire-value out)) (set-wire-value out (operation wire-value (get-wire-value in1))))
          ((null? (get-wire-value in1)) (set-wire-value in1 (inverse-op (get-wire-value out) wire-value)))
          (else #f)))
  (define (out-changed wire-value)
    (cond ((and (null? (get-wire-value in1)) (null? (get-wire-value in2))) #f)
          ((null? (get-wire-value in1)) (set-wire-value in1 (inverse-op wire-value (get-wire-value in2))))
          ((null? (get-wire-value in2)) (set-wire-value in2 (inverse-op wire-value (get-wire-value in1))))
          (else #f)))
  ((in1 'connect) in1-changed)
  ((in2 'connect) in2-changed)
  ((out 'connect) out-changed))

(define (multiplier m1 m2 p)
  (simple-three-terminal m1 m2 p * /))

(define (adder a1 a2 s)
  (simple-three-terminal a1 a2 s + - ))

(define (probe wire name)
  (define (wire-changed new-value)
    (display "New value of ") 
    (display name)
    (display ": ")
    (display new-value)
    (newline))
  ((wire 'connect) wire-changed))

(define (celcius-fahrenheit-converter c f)
  (let ((w (make-wire 9))
        (u (make-wire '()))
        (v (make-wire '()))
        (x (make-wire 5))
        (y (make-wire 32)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)))

(define C (make-wire '()))
(define F (make-wire '()))

(celcius-fahrenheit-converter C F)

(probe C 'C)
(probe F 'F)

(set-wire-value C 25)

; division by zero problem still!


  