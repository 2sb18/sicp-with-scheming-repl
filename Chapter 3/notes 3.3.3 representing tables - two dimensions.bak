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
