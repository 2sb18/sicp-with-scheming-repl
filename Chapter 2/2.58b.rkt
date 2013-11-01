#lang planet neil/sicp

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

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((eq? 0 a1) a2)
        ((eq? 0 a2) a1)
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (eq? 0 m1) (eq? 0 m2)) 0)
        ((eq? 1 m1) m2)
        ((eq? 1 m2) m1)
        (else (list m1 '* m2))))

(define (make-exponentiation b e)
  (cond ((eq? e 0) 1)
        ((eq? e 1) b)
        (else (list b '** e))))

(define (operation?-creator operator)
  (lambda (exp)
    (and (list? exp) (not (eq? #f (memq operator exp))))))

(define (operand1-creator operator)
  (lambda (exp)
    (define (loop exp)
      (if (eq? operator (car exp))
          nil
          (append (list (car exp)) (loop (cdr exp)))))
    (if (eq? operator (cadr exp))
        (car exp)
        (loop exp))))

(define (operand2-creator operator)
  (lambda (exp) 
    (let ((rest (cdr (memq operator exp))))
      (if (= (length rest) 1)
          (car rest)
          rest))))

(define sum? (operation?-creator '+))
(define addend (operand1-creator '+))
(define augend (operand2-creator '+))

(define product? (operation?-creator '*)) 
(define multiplier (operand1-creator '*))
(define multiplicand (operand2-creator '*))
  
(define exponentiation? (operation?-creator '**))
(define base (operation?-creator '**)) 
(define exponent (operation?-creator '**)) 

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)   ; only handles where exponent is a number
         (make-product (exponent exp)
                       (make-product (make-exponentiation (base exp) (- (exponent exp) 1))
                                     (deriv (base exp) var))))
        (else
         (error "unknown expression type - DERIV" exp))))


(sum? '(2 + 2))
(pl (addend '(2 + 3)))
(pl (augend '(2 + 3)))


(pl (deriv '(x + 3 * (x + y + 2)) 'x))
















      












       