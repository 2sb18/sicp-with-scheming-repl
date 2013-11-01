; returns the record of the corresponding key
; if nothing is found, returns false
(define (assoc key list-of-records)
  (cond ((null? list-of-records) #f)
        ((eq? key (caar list-of-records)) (car list-of-records))
        (else (assoc key (cdr list-of-records)))))
  

(define (lookup key table)
  (if (eq? (car table) '*table*)
      (let ((record (assoc key (cdr table))))
        (if record
            (car record)
            #f))
      (error "calling lookup procedure on a non-table" key table)))

; if record is found, replace value with new one
(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table (cons (cons key value) (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))


(define memoize
  (lambda (f)
    ((lambda (table)
       (lambda (x)
         ((lambda (previously-computed-result)
            (or previously-computed-result
                ((lambda (result)
                   (insert! x result table)
                   result) (f x)))) (lookup x table)))) (make-table))))

(define fib
  (lambda (n)
    (cond ((= n 0) 0)
          ((= n 1) 1)
          (else (+ (fib (- n 1))
                   (fib (- n 2)))))))

(define memo-fib
  (memoize fib))

(memo-fib 3)

- could we define memo-fib as (define memo-fib (memoize fib)) ?

No, because we need the memo-fib procedure to refer to itself for previous values.




        
                
                                     



