; building a semaphore

; a. in terms of mutexes

; 

; if it's set, return true
; if it's clear, return false and set to true
(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))

; if it's clear, return false
; if it's set, return true and set to false.
(define (test-and-clear! cell)
  (if (car cell)
      (begin (set-car! cell false)
             true)
      false))

(define (make-list-of-objects number-of-objects constructor)
  (if (= number-of-objects 0)
      nil
      (cons (constructor) (make-list-of-objects (- number-of-objects 1) constructor))))

(define (make-semaphore n)
  (let ((cells (make-list-of-objects n (lambda () #f))))
    (define (acquire cell)
      (if (null? cell)
          (acquire cells) ; start over at the top
          (if (test-and-set! cell)
              (acquire (cdr cell)) ; try next cell
              'ok)))
    (define (release cell)
      (if (null? cell)
          (release cells)
          (if (test-and-clear! cell)
              'ok
              (release (cdr cell)))))    
    (define (the-semaphore m)
      (cond ((eq? m 'acquire) (acquire cells))
            ((eq? m 'release) (release cells))))
    the-semaphore))

(define sema (make-semaphore 3))
(sema 'acquire)
(sema 'release)
(sema 'acquire)
(sema 'acquire)
(sema 'acquire)
(sema 'acquire)










