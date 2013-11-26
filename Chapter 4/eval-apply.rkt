

#lang planet neil/sicp 

(#%provide 
 eval
 list-of-values
 eval-if
 eval-sequence
 eval-assignment
 eval-definition
 while?
 while->if
 let?
 get-vars-of-var-expression-list
 get-exprs-of-var-expression-list
 let->combination
 let*?
 let*->nested-lets
 self-evaluating?
 variable?
 quoted?
 text-of-quotation
 tagged-list?
 assignment?
 assignment-variable
 assignment-value
 definition?
 definition-variable
 definition-value
 lambda?
 lambda-parameters
 lambda-body
 make-lambda
 if?
 if-predicate
 if-consequent
 if-alternative
 make-if
 begin?
 begin-actions
 last-exp?
 first-exp
 rest-exps
 sequence->exp
 make-begin
 application?
 operator
 operands
 no-operands?
 first-operand
 rest-operands
 cond?
 cond-clauses
 cond-else-clause?
 cond-predicate
 cond-actions
 cond->if
 expand-clauses
 true?
 false?
 make-procedure
 compound-procedure?
 procedure-parameters
 procedure-body
 procedure-environment
 enclosing-environment
 first-frame
 add-binding-to-frame!
 extend-environment
 lookup-variable-value
 set-variable-value!
 define-variable!

 the-empty-environment
 )

; the eval procedure takes an expression and the environment that it's to be
; executed it
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ; ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((let? exp) (eval (let->combination exp) env))
        ((let*? exp) (eval (let*->nested-lets exp) env))
        ((if? exp) (eval-if exp env))
        ; ((lambda? exp)
        ;  (make-procedure (lambda-parameters exp)
        ;                  (lambda-body exp)
        ;                  env))
        ; begin is used to package a sequence of expressions into
        ; a single expression
        ((while? exp) (eval (while->if exp) env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ; turn cond expression into an if expression then evaluate again
        ((cond? exp) (eval (cond->if exp) env))
        ; this is the last thing, so maybe the operator is a procedural call
        ; an application is just (function a b ..) where a and b are arguments
        ; so what if this was send (meow (if a b c) d)?

        ; ((application? exp)
        ;   the (operator exp) just takes the first element of the expression
        ;  (apply (eval (operator exp) env)
        ;         (list-of-values (operands exp) env)))
        (else
          (error "Unknown expression type -- EVAL" exp))))

; arguments is a list of arguments
; (define (apply procedure arguments)
;   (cond ((primitive-procedure? procedure)
;          (apply-primitive-procedure procedure arguments))
;         ((compound-procedure? procedure)
;          (eval-sequence
;            (procedure-body procedure)
;            (extend-environment
;              (procedure-parameters procedure)
;              arguments
;              (procedure-environment procedure))))
;         (else
;           (error
;             "Unknown procedure type -- APPLY" procedure))))

; I think exps is a list of the operands
; so this turns a list of operands into a list of values

; left-to-right evaluator
; this evaluates all the expressions in the list
; exps is just the operands, so no-operands just checks to see if exps is null 
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

(define (while? exp) (tagged-list? exp 'while))

(define (while->if exp)
  (make-if (cadr exp)
    (make-begin (list (cddr exp)
                      exp))))


(define (let? exp) (tagged-list? exp 'let))
(define (get-vars-of-var-expression-list var-expression-list)
  (if (null? var-expression-list)
    '()
    (cons (caar var-expression-list )
          (get-vars-of-var-expression-list (cdr var-expression-list)))))
(define (get-exprs-of-var-expression-list var-expression-list)
  (if (null? var-expression-list)
    '()
    ; !!! pretty sure var-expression pair is just a cons, not a list
    (cons (cdar var-expression-list )
          (get-exprs-of-var-expression-list (cdr var-expression-list)))))

; the exp in this case is a cons-chain, let.var-exp-list.body
; special form of let is let.var.var-exp-list.body where body is binded
; to var, so that it can call itself
(define (let->combination exp)
  ; if there's only three elements in the let, it's normal form
  (if (not (pair? (cddr exp)))
    (cons (make-lambda (get-vars-of-var-expression-list (cadr exp))
                       (cddr exp))
          (get-exprs-of-var-expression-list var-expression-list))
    ; we got the crazy special let form )
    (make-begin (list 
                  (cons 'define
                        (cons (cons (cadr exp) (get-vars-of-var-expression-list (caddr exp)))
                              (cdddr exp)))
                  (cons (cadr exp) (get-exprs-of-var-expression-list (caddr exp)))))))

; the exp in this case is a cons-chain, let*.var-exp-list.body
(define (let*? exp) (tagged-list? exp 'let*))
(define (let*->nested-lets exp)
  ; if there's only one var-expression pair left, just turn the let* into a
  ; let
  (if (= 1 (length (cadr exp)))
    (cons 'let (cdr exp))
    (cons 'let (list (cadr exp))
          (cons 'let* (cons (cdadr exp) (cddr exp))))))

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
    (make-lambda (cdadr exp)    ; formal parameters
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

; only requirement for it to be an application is for
; it to be a pair? Easy!
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
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
        ; is the (<test> => <recipient>) syntax is being used?
        (if (and (= 3 (length first)) (eq? '=> (list-ref first 1)))
          ; kinda ugly but we're doing the evaluation right here
          (let ((predicate (eval (car first))))
            (if (true? predicate)
              ((caddr first) predicate)
              (expand-clauses rest)))
          (make-if (cond-predicate first)
                   (sequence->exp (cond-actions first))
                   (expand-clauses rest)))))))

;;;;;;;;;
; TESTING PREDICATES
;;;;;;;;;
; anything that isn't an explicit false is true
(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))

;;;;;;;;;
; REPRESENTING PROCEDURES
;;;;;;;;;
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

;;;;;;;;;
; OPERATIONS ON ENVIRONMENTS
;;;;;;;;;
; Exercise 4.11.  Instead of representing a frame as a pair of lists, we can represent a frame as a list of bindings, where each binding is a name-value pair. Rewrite the environment operations to use this alternative representation. 

; an environment is a list of frames
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())


; we add variables to frames by pushing them onto the top
; off both parts of the frame cons
(define (add-binding-to-frame! var val frame)
  (set! frame (cons (cons var val) frame)))

(define (extend-environment frame base-env)
  (cons frame base-env))

(define (lookup-variable-value var env)
  (define (lookup-variable-in-frame var frame)
    (cond ((null? frame) '())
          ((eq? var (caar frame)) (cdar frame))
          (else (lookup-variable-in-frame var (cdr frame)))))
  (let ((value (lookup-variable-in-frame var (first-frame env))))
    (if (not (null? value))
      value
      (if (eq? (enclosing-environment env) the-empty-environment)
        '() ; variable wasn't found
        (lookup-variable-value var (enclosing-environment env))))))

(define var-expression-list '())
(define (set-variable-value! var val)
  'meow)

(define (define-variable! var val)
  'meow)
