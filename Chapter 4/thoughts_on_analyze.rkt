

; our eval function looks like this:
(define (our-eval exp env)
  ((analyze exp) env))

; our driver loop evaluates expressions in the global environment,
; so it would look like this...
(our-eval input-expression the-global-environment)

; so what if our expression is this?
3

; the analyze procedure creates a new function that tries to get to answer
; in the expression as quickly as possible.

; this was our old application evaluation in our-eval
((pair? exp)
 (our-apply (our-eval (car exp) env)
            (list-of-values (cdr exp) env)))
; this takes the pair, and evaluates the car through our-eval.
; the cdr of the expression it takes to be a list. It maps this
; list through our-eval.
; then it takes this produces pair, and sends it into our-apply

; now how can we make this better with analyze?
; analyze has to return a procedure that takes an environment
; so can we analyze each of the 
((pair? exp)
 (lambda (env)
   (our-apply ((analyze (car exp)) env)
              (map analyze (cdr exp)))))








