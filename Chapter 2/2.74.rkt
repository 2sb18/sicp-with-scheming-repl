#lang planet neil/sicp

(define (square n)
  (* n n))

; print list, can be nested
; makes all the spacing correct!
(define (pl n)
  (define (in-loop n space?)
    (cond ((null? n) 0)
          ((not (pair? n)) (if space? (display " ")) (display n))
          ((list? (car n)) (out-loop (car n) #t) (in-loop (cdr n) #t))
          (else (if space? (display " ")) (display (car n)) (in-loop (cdr n) #t)))) 
  (define (out-loop n space?)
    (cond ((list? n) (if space? (display " ")) (display "(") (in-loop n #f) (display ")"))
          (else (in-loop n #f))))
  (out-loop n #f) (display "\n"))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(put 'record 'acme-re

(define (install-acme-division-package)
  ;; internal procedures and files
  (define (get-record employee-name personnel-file)
    yadda yadda return record) ; returns false if nothing found
  (define (get-salary employee-record)
    yadda yadda return salary)
  ;; interface to the rest of the system
  (put 'get-record 'acme get-record)
  (put 'get-salary 'acme get-salary)
  'done)
(install-acme-division-package)

; generic-file will have a type
(define (get-record employee-name personnel-file)
  ((get 'get-record (type-tag generic-file)) employee-name (contents generic-file)))

; record needs to have the type of the division
(define (get-salary employee-record)
  ((get 'get-salary (type-tag employee-record)) (contents employee-record)))

(define (find-employee-record employee-name division-list)
  (if (null? division-list)
      (error "employee can't be found -- FIND-EMPLOYEE-RECORD" employee-name)
      (let ((employee-record (get-record (car division-list) employee-name)))
        (if (eq? #f employee-record)
            (find-employee-record employee-name (cdr division-list))))))

; d. if insatiable takes over a new company, we only have to install
; a new division package






