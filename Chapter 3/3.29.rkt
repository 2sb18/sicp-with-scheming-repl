(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)
(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

; or gate as a combination of 

(define (or-gate o1 o2 output)
  (let ((a (make-wire))
        (b (make-wire))
        (c (make-wire)))
    (inverter o1 a)
    (inverter o2 b)
    (inverter c output)
    (and-gate a b c)
    'ok))

; delay time is two inverter-delays and one and-gate-delay

