
#lang planet neil/sicp 

; the eval procedure takes an expression and the environment that it's to be
; executed it
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ; begin is used to package a sequence of expressions into
        ; a single expression
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ; turn cond expression into an if expression then evaluate again
        ((cond? exp) (eval (cond->if exp) env))
        ; this is the last thing, so maybe the operator is a compound procedure
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
          (error "Unknown expression type -- EVAL" exp))))

; arguments is a list of arguments
(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
          (error
            "Unknown procedure type -- APPLY" procedure))))

; I think exps is a list of the operands
; so this turns a list of operands into a list of values

; left-to-right evaluator
(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (let ((left (eval (first-operand exps) env)))
      (let ((right (list-of-values (rest-operands exps) env)))
        (cons left right)))))

; right-to-left evaluator
(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (let ((right (list-of-values (rest-operands exps) env)))
      (let ((left (eval (first-operand exps) env)))
        (cons left right)))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
    (eval (if-consequent exp) env)
    (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps) env))))

; set-variable-value puts the variable and value intto the
; designated environment
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                   (eval (definition-value exp) env)
                   env)
  'ok)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SPECIFICATION OF THE SYNTAX 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; only numbers and strings are self-evaluating
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

; variables are represented by symbols
(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

; is exp of the form (tag a b c d ...)
(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    false))

; something is an assignment if it's (set! a b)
(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
    (caddr exp)
    (make-lambda (cdadr exp)     
                 (cddr exp))))   
(define (definition-value exp)
  (if (symbol? (cadr exp))
    (caddr exp)
    (make-lambda (cdadr exp)    ; formal parametersi
                 (cddr exp))))  ; body

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

; will accept something like (if meow boo hoo)
(define (if? exp) (tagged-list? exp 'if))
; this would return meow
(define (if-predicate exp) (cadr exp))
; this would return boo
(define (if-consequent exp) (caddr exp))
; if hoo exists, execute it, if not, return false 
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
    (cadddr exp)
    'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))


