#lang planet neil/sicp 

(#%require "../pretty-list-printer.rkt")

(#%provide 
 our-eval
 eval-if
 eval-sequence
 eval-assignment
 eval-definition
 while->if
 get-vars-of-var-expression-list
 get-exprs-of-var-expression-list
 let->combination
 let*->nested-lets
 tagged-list?
 definition-variable
 definition-value
 make-lambda
 make-if
 sequence->exp
 make-begin
 expand-clauses
 true?
 false?
 make-procedure
 frame-bindings
 add-binding-to-frame!
 extend-environment
 lookup-variable-value
 set-variable-value!
 define-variable!
 unbind!
 empty-environment
 the-global-environment
 its
 driver-loop
 plp
 scan-out-defines 
 user-print
 pits
 let-and-set
 )

(define (apply-in-underlying-scheme procedure arguments)
  (apply procedure arguments))


; the our-eval procedure takes an expression and the environment that it's to be
; executed in
(define (our-eval exp env)
  ((analyze exp) env))

; analyze has to return a procedure that takes an environment variable
; as an argument
(define (analyze exp)
  (cond 
    ; self-evaluating?
    ((number? exp) (lambda (env) exp))
    ((string? exp) (lambda (env) exp))
    ; variables are represented by symbols
    ((symbol? exp) (lambda (env) (lookup-variable-value exp env))) ; good!
    ; get the text of the quotation
    ((tagged-list? exp 'quote) 
     (let ((qval (cadr exp))) (lambda (env) qval)))
    ((tagged-list? exp 'set!) (analyze-assignment exp))
    ((tagged-list? exp 'define) (analyze-definition exp))
    ((tagged-list? exp 'make-unbound!) 
     (let ((variable-name (cadr exp)))
       (lambda (env) (unbind! variable-name env))))
    ((tagged-list? exp 'let) (analyze (let->combination exp)))
    ((tagged-list? exp 'let*) (analyze (let*->nested-lets exp)))
    ((tagged-list? exp 'letrec) (analyze (let-and-set (cadr exp) (cddr exp))))
    ((tagged-list? exp 'if) (analyze-if exp))
    ; a lambda expression looks like: '(lambda parameters-list body-1 body-2 .. body-n)
    ((tagged-list? exp 'lambda) (analyze-lambda exp))
    ; (make-procedure (cadr exp)
    ;                 (cddr exp)
    ;                 env))
    ; begin is used to package a sequence of expressions into
    ; a single expression
    ((tagged-list? exp 'while) (analyze (while->if exp)))
    ((tagged-list? exp 'begin)
     (analyze-sequence (cdr exp)))
    ; turn cond expression into an if expression then evaluate again
    ((tagged-list? exp 'cond) (analyze (expand-clauses (cdr exp))))
    ; now we want to see if it's an application, which is the same thing
    ; as a function call
    ; this is the last thing, so maybe the operator is a procedural call
    ; an application is just (function a b ..) where a and b are arguments
    ; so what if this was send (meow (if a b c) d)?
    ((pair? exp)
     (analyze-application exp))
    (else
      (error "Unknown expression type -- EVAL" exp))))

; an application looks like (meow woof cat ... bat)
; where meow could be an expression that returns a function
; and woof cat ... bat are arguments for the application
(define (analyze-application exp)
  ; analyze all the elements of the expression
  (let ((fproc (analyze (car exp)))
        (aprocs (map analyze (cdr exp))))
    (lambda (env)
      (execute-application (fproc env)
                           (map (lambda (aproc) (aproc env))
                                aprocs)))))
(define (execute-application proc args)
  (cond ((tagged-list? proc 'primitive)
         (apply-primitive-procedure proc args))
        ((tagged-list? proc 'procedure)
         ((caddr proc)
          (extend-environment (cadr proc)
                              args
                              (cadddr proc))))
        (else
          (error
            "Unknown procedure type -- EXECUTE-APPLICATION"
            proc))))

(define (analyze-if exp)
  (let ((predicate-prod (analyze (cadr exp)))
        (if-true-prod (analyze (caddr exp)))
        (if-false-prod (analyze (if (not (null? (cdddr exp)))
                                  (cadddr exp)
                                  'false))))
    (lambda (env)
      (if (true? (predicate-prod env))
        (if-true-prod env)
        (if-false-prod env)))))


; exp looks like (if predicate do-if-true do-if-false)
(define (eval-if exp env)
  (if (true? (our-eval (cadr exp) env))
    (our-eval (caddr exp) env)
    ; if there's an alternative, run it
    (our-eval 
      (if (not (null? (cdddr exp)))
        (cadddr exp)
        'false)
      env)))

; exps should just be a list of expressions
(define (analyze-sequence exps)
  ; this will return a procedure that runs proc1 and then proc2
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
      first-proc
      (loop (sequentially first-proc (car rest-procs))
            (cdr rest-procs))))
  ; put each expression through our analyze function
  (let ((procs (map analyze exps)))
    (if (null? procs)
      (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (eval-sequence exps env)
  (cond ((null? (cdr exps)) (our-eval (car exps) env))
        (else (our-eval (car exps) env)
              (eval-sequence (cdr exps) env))))

; return a function that takes an environment as an argument
(define (analyze-assignment exp)
  (let ((variable-name (cadr exp))
        (variable-proc (analyze (caddr exp))))
    (lambda (env)
      (set-variable-value! variable-name
                           (variable-proc env)
                           env))))

; set-variable-value puts the variable and value into the
; designated environment
; exp would look like (set! variable-name value)
(define (eval-assignment exp env)
  (set-variable-value! (cadr exp)
                       (our-eval (caddr exp) env)
                       env)
  'ok)

(define (analyze-definition exp)
  (let ((definition-name (definition-variable exp))
        (definition-procedure (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! definition-name
                        (definition-procedure env)
                        env))))


(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (our-eval (definition-value exp) env)
                    env)
  'ok)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SPECIFICATION OF THE SYNTAX 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (while->if exp)
  (make-if (cadr exp)
           (make-begin (list (cddr exp)
                             exp))))

(define (get-vars-of-var-expression-list var-expression-list)
  (if (null? var-expression-list)
    '()
    (cons (caar var-expression-list )
          (get-vars-of-var-expression-list (cdr var-expression-list)))))
(define (get-exprs-of-var-expression-list var-expression-list)
  (if (null? var-expression-list)
    '()
    (cons (cadar var-expression-list )
          (get-exprs-of-var-expression-list (cdr var-expression-list)))))

; the exp is a list, (let var-exp-list body)
;                  = (let ((a 3) (b 4)) body)
; lets get turned into a application of a lambda
(define (let->combination exp)
  (cons (make-lambda (get-vars-of-var-expression-list (cadr exp))
                     (cddr exp))
        (get-exprs-of-var-expression-list (cadr exp))))

(define (let*->nested-lets exp)
  ; if there's only one var-expression pair left, just turn the let* into a
  ; let
  (if (= 1 (length (cadr exp)))
    (cons 'let (cdr exp))
    (list 'let 
          (list (caadr exp))
          (list 'let*
                (cdadr exp)
                (caddr exp)))))

; is exp of the form (tag a b c d ...)
(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    false))

; ex. exp = (define x 3), this should return x
(define (definition-variable exp)
  (if (symbol? (cadr exp))
    ; if it's a variable
    (cadr exp)
    ; if it's a procedure
    (caadr exp)))   
(define (definition-value exp)
  (if (symbol? (cadr exp))
    ; if symbol
    (caddr exp)
    ; if procedure
    (make-lambda (cdadr exp)    ; formal parameters
                 (cddr exp))))  ; body

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((null? (cdr seq)) (car seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

; clauses would look like:
; (a b) (c d) (else e)
(define (expand-clauses clauses)
  (if (null? clauses)
    'false
    ; first is the first clause
    (let ((first (car clauses))
          (rest (cdr clauses)))
      (if (eq? 'else (car first))
        (if (null? rest)
          (sequence->exp (cdr first))
          (error "ELSE clause isn't last -- COND->IF" clauses))
        ; is the (<test> => <recipient>) syntax is being used?
        (if (and (= 3 (length first)) (eq? '=> (list-ref first 1)))
          ; kinda ugly but we're doing the evaluation right here
          (let ((predicate (our-eval (car first))))
            (if (true? predicate)
              ((caddr first) predicate)
              (expand-clauses rest)))
          (make-if (car first)
                   (sequence->exp (cdr first))
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

; parameters is (cadr exp), body is (cddr),
(define (analyze-lambda exp)
  (let ((new-body (analyze-sequence (scan-out-defines (cddr exp)))))
    (lambda (env)
      (list 'procedure (cadr exp) new-body env))))


; (make-procedure (cadr exp)
;                 (cddr exp)
;                 env))

; right, each procedure points to an env
; parameters is a list of parameters
; body is a list structure
(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))

; scan-out-defines takes a procedure body and returns
; an equivalent one that has not internal definitions

; (lambda <vars>
;   (define u <e1>)
;   (define v <e2>)
;   <e3>)
;
; would be transformed into
;
; (lambda <vars>
;   (let ((u '*unassigned*)
;         (v '*unassigned*))
;     (set! u <e1>)
;     (set! v <e2>)
;     <e3>))
(define (scan-out-defines procedure-body)
  (define (get-bindings procedure-body)
    (if (null? procedure-body)
      '()
      (if (and (pair? (car procedure-body)) 
               (equal? 'define (caar procedure-body)))
        (cons (cdar procedure-body)
              (get-bindings (cdr procedure-body)))
        (get-bindings (cdr procedure-body)))))
  (define (get-rest procedure-body)
    (if (null? procedure-body)
      '()
      (if (or (not (pair? (car procedure-body)))
              (not (equal? 'define (caar procedure-body))))
        (cons (car procedure-body)
              (get-rest (cdr procedure-body)))
        (get-rest (cdr procedure-body)))))
  (if (not (pair? procedure-body))
    procedure-body
    (let ((the-bindings (get-bindings procedure-body))
          (the-rest (get-rest procedure-body)))
      (if (= 0 (length the-bindings))
        procedure-body ; don't need to scan out anything
        ; the-bindings))))
        (let-and-set the-bindings the-rest)))))

; create a let and set expression
; we send it a list of bindings. 
; ex.  ((even? (lambda (n) yadda yadda)) (odd? (lambda (n) yadda badda)))
; example of inner-expressions: ((even 4) (odd 5))
(define (let-and-set bindings inner-expressions)
  (define (create-list-of-assignments bindings)
    ; not sure if this double quote thing is cool
    (map (lambda (x) (list (car x) ''*unassigned*)) bindings))
  (define (create-list-of-sets! bindings)
    (map (lambda (x) (list 'set! (car x) (cadr x))) bindings))
  (list (append '(let) (list (create-list-of-assignments bindings))
                (create-list-of-sets! bindings)
                inner-expressions)))

;;;;;;;;;
; OPERATIONS ON ENVIRONMENTS
;;;;;;;;;
; Exercise 4.11.  Instead of representing a frame as a pair of lists, we can represent a frame as a list of bindings, where each binding is a name-value pair. Rewrite the environment operations to use this alternative representation. 

; an environment is a list of frames
(define (frame-bindings frame)
  (car frame))
(define (enclosing-environment env)
  (cdr env))
(define (empty-environment)
  (cons '() '()))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons (cons var val) (car frame))))

(define (make-frame variables values base-env)
  (let ((frame-to-return (cons '() base-env)))
    (define (make-frame-iter variables values)
      (if (null? variables)
        frame-to-return
        (begin (add-binding-to-frame! (car variables) (car values) frame-to-return)
               (make-frame-iter (cdr variables) (cdr values))))) 
    (make-frame-iter variables values)))

; extend-environment should take a list of vars and a list of values
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
    (make-frame vars vals base-env) 
    (if (< (length vars) (length vals))
      (error "Too many arguments supplied" vars vals)
      (error "Too few arguments supplied" vars vals))))

(define (return-variable-value var env)
  (define (lookup-variable-in-frame var frame)
    (cond ((null? frame) '())
          ((eq? var (caar frame)) (car frame))
          (else (lookup-variable-in-frame var (cdr frame)))))
  (let ((variable-value (lookup-variable-in-frame var (frame-bindings env))))
    (if (not (null? variable-value))
      variable-value
      (if (eq? (enclosing-environment env) '())
        '() ; variable-value wasn't found
        (return-variable-value var (enclosing-environment env))))))

(define (lookup-variable-value var env)
  (let ((variable-value (return-variable-value var env)))
    (if (null? variable-value)
      (error "couldn't find variable -- LOOKUP-VARIABLE-VALUE" var env) 
      (if (eq? (cdr variable-value) '*unassigned*)
        (error "value of variable is unassigned -- LOOKUP-VARIABLE-VALUE" var env)
        (cdr variable-value)))))

(define var-expression-list '())

(define (set-variable-value! var val env)
  ; first we want to get the variable to set
  (let ((variable-value (return-variable-value var env)))
    (if (null? variable-value)
      (error "couldn't find variable -- SET-VARIABLE-VALUE!" var val env)
      (set-cdr! variable-value val))))

; add a binding to the current frame given by env
(define (define-variable! var val env)
  ; we only want to search the first frame
  (let ((first-frame-only (cons (frame-bindings env) '())))
    (let ((variable-value (return-variable-value var first-frame-only)))
      (if (null? variable-value)
        (add-binding-to-frame! var val env)
        (set-cdr! variable-value val)))))

; but on the other hand, you can set! variables in enclosing
; frames so why can't you unbind them?
; being able to unbind enclosing frames is dangerous but maybe
; also powerful, let's allow for it
; return true if it happened, error if it
; didn't
(define (unbind! var env)
  ; returns true or false depending on whether unbinding
  ; was successful 
  (define (unbind-in-frame frame)
    (cond ((null? frame) #f)
          ((null? (cdr frame)) #f)
          ((eq? var (caadr frame))
           (begin (set-cdr! frame (cddr frame))
                  #t))
          (else (unbind-in-frame (cdr frame)))))
  (cond ((null? env)
         (error "couldn't find variable -- UNBIND!" var env))
        ; if the frame doesn't have any bindings
        ((null? (car env)) (unbind! var (cdr env)))
        ; if the var is the first in the frame...
        ((eq? var (caaar env)) (set-car! env (cdar env)))
        ((unbind-in-frame (frame-bindings env)))
        (else (unbind! var (cdr env)))))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '= =)
        (list 'display display)
        (list 'null? null?)))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (setup-environment)
  (let ((initial-env
          (extend-environment (primitive-procedure-names)
                              (primitive-procedure-objects)
                              (empty-environment))))

    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))
(define the-global-environment (setup-environment))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme    
    (cadr proc) args))

; print input to system
(define (pits exp)
  (if (eq? exp 'the-global-environment)
    (display the-global-environment)
    (let ((output (our-eval exp the-global-environment)))
      (user-print output)
      (newline))))

; input to system
(define (its exp)
  (our-eval exp the-global-environment))

(define (driver-loop)
  (newline)
  (display "M-eval> ")
  (let ((input (read)))
    ; (read) seems to take the input and figure out what kind of type it
    ; is, so that our eval procedure can handle it properly
    ; typing in "hello" (without the quotations) results in a symbol sent
    ; typing in "'(1 2 3)" results in a list sent
    (if (eq? input 'the-global-environment)
      (display the-global-environment)
      (let ((output (our-eval input the-global-environment)))
        (newline)
        (user-print output))))
  (driver-loop))

(define (user-print object)
  (cond ((tagged-list? object 'procedure)
         ; why do we have this special condition for displaying procedures?
         ; cause a procedure contains a pointer to an environment, so we'd get 
         ; a big loop if we allowed plp to print the environment
         (plp (list 'procedure
                    (cadr object)
                    (caddr object))))
        ((pair? object) (plp object))
        (else (display object))))

