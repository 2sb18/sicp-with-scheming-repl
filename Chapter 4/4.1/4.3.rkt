
#lang planet neil/sicp 

; the eval procedure takes an expression and the environment that it's to be
; executed it

; ASSUMPTIONS
; 
; get returns a function if there is one, and #f if there isn't

; the car of the exp is the type, so for a number, the expression
; would be (number 3) and for a string, the expression would be
; (string "hello")
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((get (car exp)) ((get (car exp)) exp env))
        (else (apply (eval (operator exp) env)
                     (list-of-values (operands exp) env)))))

(put 'quote (lambda (exp env) (text-of-quotation exp)))
(put 'set! (lambda (exp env) (eval-assignment exp env)))
(put 'define (lambda (exp env) (eval-definition exp env)))
(put 'if (lambda (exp env) (eval-if exp env)))
(put 'begin (lambda (exp env) (eval-sequence (begin-actions exp) env)))
(put 'cond (lambda (exp env) (eval (cond->if exp) env)))
(put 'lambda (lambda (exp env) (make-procedure (lambda-parameters exp)
                                                     (lambda-body exp)
                                                     env)))

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

; Louis made it so an application starts with call
; ex (call factorial 3)
; only first three defines had to change
(define (application? exp) (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))

; takes in an expression like:
; (cond (a b)
;       (c d)
;       (else e))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

; clauses would look like:
; (a b) (c d) (else e)
(define (expand-clauses clauses)
  (if (null? clauses)
    'false
    ; first is the first clause
    (let ((first (car clauses))
          (rest (cdr clauses)))
      (if (cond-else-clause? first)
        (if (null? rest)
          (sequence->exp (cond-actions first))
          (error "ELSE clause isn't last -- COND->IF" clauses))
        (make-if (cond-predicate first)
                 (sequence->exp (cond-actions first))
                 (expand-clauses rest))))))

