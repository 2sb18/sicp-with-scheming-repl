#lang planet neil/sicp

; let's see how explicit dispatch, data-directed-sytle, and message-
; passing-style are effected when new types or new operations have to
; be added 

; explicit dispatch

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        <more rules can be added here>
        (else (error "unknown expression type -- DERIV" exp))))

; the operation is (deriv), the types are +, *. 
; if a new operation is to be added, we write a new procedure
; for that operation.
; if a new type is to be added, we add a conditional to each
; procedure for that type

; data-directed style

(define (deriv exp var)
   (cond ((number? exp) 0)
         ((symbol? exp) (if (eq? exp var) 1 0))
         (else ((get 'deriv (operator exp)) (operands exp)
                                            var))))

(define (install-sum-package)
  ;; internal procedures
  (define (derive-sum sum var) ; ex. sum = '(3 x)
    (make-sum (deriv (car sum) var)
                   (deriv (cadr sum) var)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag '+ x))
  (define (make-sum a1 a2)
    (cond ((eq? 0 a1) a2)
          ((eq? 0 a2) a1)
          ((eq? a1 a2) (make-product 2 a1))
          (else (tag (list a1 a2)))))
  (put 'make-sum '+ make-sum)
  (put 'deriv '+ derive-sum)
  'done)
(install-sum-package)
(define (make-sum a1 a2)
  ((get 'make-sum '+) a1 a2))

; for each operation we'd have a procedure, like the (deriv)
; procedure
; for each type/operation combo, we'd have to (put) a procedure
; to the table.

; message-passing-style

; have to constructor procedure for each type. at first I thought
; this would be weird, but it turned out to be nice. information
; hiding, and succinct. You don't have to do the put/get like you
; do in data-directed style.

(define (deriv exp var)
  (exp 'deriv var))

(define (make-sum exp1 exp2)
  (define (dispatch op . args)
    (cond 
      ((eq? op 'deriv) (make-sum (deriv exp1 (car args)) (deriv exp2 (car args))))
      ((eq? op 'show) (list '+ (show exp1) (show exp2)))))
  (cond ((eq? 0 (show exp1)) exp2)
        ((eq? 0 (show exp2)) exp1)
        ((eq? (show exp1) (show exp2)) (make-product (make-number 2) exp))
        (else dispatch)))

; in a system where new types are added (like sums in the examples above),
; the message-passing style seems best. If a new type is added, none of
; the old types have to be changed.

; in a system where new operations are added (like deriv in the examples above),
; the data-directed style would be best. Packages could be installed for each
; new operation.


