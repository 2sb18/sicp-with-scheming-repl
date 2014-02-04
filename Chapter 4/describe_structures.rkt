#lang planet neil/sicp
(#%require "eval-apply.rkt")

(#%require "test-eval-apply.rkt")

(newline)

(display "this is a var-expression-list: '((a 4) (b 3) (c 2))\n")
(define var-expression-list '((a 4) (b 3) (c 2)))
(plp var-expression-list)
(newline)

(display "this is the var-expression-list run through get-vars-of-var-expression-list\n")
(run-and-check "(get-vars-of-var-expression-list var-expression-list)"
               (get-vars-of-var-expression-list var-expression-list)
               '(a b c))

(display "this is the var-expression-list run through get-exprs-of-var-expression-list\n")
(run-and-check "(get-exprs-of-var-expression-list var-expression-list)"
               (get-exprs-of-var-expression-list var-expression-list)
               '(4 3 2))

(run-and-check "(make-lambda 'parameters 'body)"
               (make-lambda 'parameters 'body)
               '(lambda parameters body))

(run-and-check "(make-procedure 'parameters 'body 'env)"
               (make-procedure 'parameters 'body 'env)
               '(procedure parameters body env))

(display "Notice that a procedure is a lambda with a pointer to an environment.\n")
(newline)

(display "Next we'll scan out the defines of a body that is just '(x)\n")
(run-and-check '(scan-out-defines '(x)) (scan-out-defines '(x)) '(x))

(run-and-check "((lambda (x) x) 3)"
               ((lambda (x) x) 3)
               3)

(run-and-check "(let ((a 3)) a)"
               (let ((a 3)) a)
               3)

(its-and-check '(let* ((a 3)) a) 3)

(define implicit-let*-nest '(let* ((a 3) (b (* a 2))) b))
(display "Here's implicit let* nesting: ") (display implicit-let*-nest) (newline)
(display "Here it is run through the let*->nested-lets procedure\n")
(plp (let*->nested-lets implicit-let*-nest))
(its-and-check implicit-let*-nest 6)

(its-and-check '(let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (* x z)) 39)

(define procedure-body-without-defines '((meow I cool) (yes cool)))
(display "Here's our procedure without defines: ")
(display procedure-body-without-defines) (newline)
(define after-scan-out-without-defines (scan-out-defines procedure-body-without-defines))
(display "This is after scan-out-defines:\n")
(plp after-scan-out-without-defines)
(newline)

(define procedure-body-with-defines '((define u 3) (define v 4) (* u v)))
(display "Here's our procedure with defines: ")
(display procedure-body-with-defines) (newline)
(define after-scan-out (scan-out-defines procedure-body-with-defines))
(display "This is after scan-out-defines:\n")
(plp after-scan-out)
(newline)

(its-and-check '((lambda (x)
                   (* x (meow x))
                   (define (meow x)
                     (* x x)))
                 4)
               64)


