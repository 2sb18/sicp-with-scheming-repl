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
               '(lambda parameters . body))

(run-and-check "(make-procedure 'parameters 'body 'env)"
               (make-procedure 'parameters 'body 'env)
               '(procedure parameters body env))

(display "Notice that a procedure is a lambda with a pointer to an environment.\n")
(newline)

(display "Next we'll scan out the defines of a body that is just '(x)\n")
(run-and-check '(scan-out-defines '(x)) (scan-out-defines '(x)) '(x))

(its-and-check '((lambda (x) x) 3) 3)

(its-and-check '(let ((a 3)) a) 3)

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

(its-and-check '((lambda (x) x) 3) 3)

(its-and-check '(define meow (lambda (x) x)) 'ok)
(its-and-check '(meow 3) 3)

(its-and-check '(define double-meow (lambda (x) 3 x)) 'ok)

(its-and-check '(double-meow 4) 4)

(its-and-check '(let ((a 0)) (set! a 3) a) 3)

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

; here's what the transformation should look like
(its-and-check '(define transformed (lambda (x) (let ((u '*unassigned*)) (set! u 3) (* x u)))) 'ok)
(user-print (its 'transformed)) 
(its-and-check '(transformed 4) 12)

; here's what our transformation actually looks like
(its-and-check '(define before (lambda (x) (define u 3) (* x u))) 'ok)
(user-print (its 'before))
(its-and-check '(before 4) 12)

(its-and-check '(define scan-test (lambda (x) (* x u v) (define u 3) (define v 4))) 'ok)
(its-and-check '(scan-test 2) 24)

; #####################
; LETREC
; #####################

; THE REC IS FOR RECURSIVE!!!!!

; let's check scan-out-defines:

; (plp (scan-out-defines
;        '(
;          (define m 3)
;          (* m 4))))

; (its '(letrec ((meow 3) (hello 4)) (* 3 4)))


; does this already work?
; this ....
; (its '(define (f x)
;         (define (even? n)
;           (if (= n 0)
;             true
;             (odd? (- n 1))))
;         (define (odd? n)
;           (if (= n 0)
;             false
;             (even? (- n 1))))
;         (even? x)))
;
; (pits 'f)

; (its '(f 5))


; (its '(define (f x)
;         (letrec ((even?
;                    (lambda (n)
;                      (if (= n 0)
;                        true
;                        (odd? (- n 1)))))
;                  (odd?
;                    (lambda (n)
;                      (if (= n 0)
;                        false
;                        (even? (- n 1))))))
;           (even? x))))
;
; (its '(f 5))

;
; ; should turn into this ...
; (define (f x)
;   (let ((times-2 'unassigned)
;         (times-3 'unassigned))
;     (set! times-2 (lambda (n) (* n 2)))
;     (set! times-3 (lambda (n) (* n 3)))
;     (times-2 (times-3 x))))
;
;
; (define hello 3)
;
; (set! hello 3)
;

