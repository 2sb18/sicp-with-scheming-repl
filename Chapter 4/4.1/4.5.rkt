
;Exercise 4.5.  Scheme allows an additional syntax for cond clauses, (<test> => <recipient>). If <test> evaluates to a true value, then <recipient> is evaluated. Its value must be a procedure of one argument; this procedure is then invoked on the value of the <test>, and the result is returned as the value of the cond expression. For example

;(cond ((assoc 'b '((a 1) (b 2))) => cadr)
;      (else false))

;returns 2. Modify the handling of cond so that it supports this extended syntax. 

; MY ANSWER, had to modify the expand-clauses function

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
