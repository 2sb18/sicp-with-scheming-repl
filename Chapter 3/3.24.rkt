(define (make-table same-key?)
  (let ((table (list '*table*)))
    ; returns the record of the corresponding key
    ; if nothing is found, returns false
    (define (assoc key list-of-records)
      (cond ((null? list-of-records) #f)
            ((same-key? key (caar list-of-records)) (car list-of-records))
            (else (assoc key (cdr list-of-records)))))
    
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr table))))
        (if (not subtable)
            #f
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  #f)))))
    
    ; if record is found, replace value with new one
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable (cons (cons key-2 value) (cdr subtable)))))
            (set-cdr! table (cons (list key-1 (cons key-2 value)) (cdr table))))
        'ok))
    (lambda (proc)
      (cond ((eq? proc 'lookup) lookup)
            ((eq? proc 'insert!) insert!)
            (else (error "procedure does not exist for table" proc table))))))

(define table1 (make-table eq?))
((table1 'insert!) 'a 'b 7)
((table1 'insert!) 'c 'd 6)
((table1 'lookup) 'a 'b)
((table1 'lookup) 'c 'd)
((table1 'lookup) 'b 'a)

(define (not-going-to-happen x y)
  #f)

(define table1 (make-table not-going-to-happen))
((table1 'insert!) 'a 'b 7)
((table1 'insert!) 'c 'd 6)
((table1 'lookup) 'a 'b)
((table1 'lookup) 'c 'd)
((table1 'lookup) 'b 'a)



