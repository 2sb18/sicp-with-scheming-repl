; when we run the following

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
      first-proc
      (loop (sequentially first-proc (car rest-procs))
            (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
      (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

; where exps = (exp1 exp2), we'll get something like

(lambda (env)
  (analyzed-exp1 env)
  (analyzed-exp2 env))

; so nice. running that procedure will be really efficient

; with exp = (exp1 exp2 exp3) we'd get

(lambda (env)
  ((lambda (env)
     (analyzed-exp1 env)
     (analyzed-exp2 env)) env)
  (analyzed-exp3 env))

; procedures within procedures within procedures


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; when we run the following

(define (analyze-sequence exps)
  (define (execute-sequence procs env)
    (cond ((null? (cdr procs)) ((car procs) env))
          (else ((car procs) env)
                (execute-sequence (cdr procs) env))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
      (error "Empty sequence -- ANALYZE"))
    (lambda (env) (execute-sequence procs env))))

; where exps = (exp1 exp2), we'll get something like

(lambda (env) (execute-sequence (analyzed-exp1 analyzed-exp2) env))

; we still have to run that loop, so there's extra execution stuff that we
; have to do. it will take longer.

; what does the-scheming-repl do??? let's check!!!!

; function analyze_sequence(sequence_of_expressionTrees) {
;   "use strict";
;   // first, analyze all the expressionTrees
;   var analyzed_expressions = [];
;   for (var i = 0; i < sequence_of_expressionTrees.length; i++) {
;     analyzed_expressions.push(analyze(sequence_of_expressionTrees[i]));
;   }
;   return function(env) {
;     var value_to_return;
;     for (var i = 0; i < analyzed_expressions.length; i++) {
;       value_to_return = analyzed_expressions[i](env);
;     }
;     return value_to_return;
;   };
; }

; ya, it has the same prob

; how do we get a structure to return like 
; return function(env) {
;   
