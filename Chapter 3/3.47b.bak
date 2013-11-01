; building a semaphore

; a. in terms of mutexes

(define (make-mutex)
  (let ((cell (list #f)))            
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))
(define (clear! cell)
  (set-car! cell false))

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))

(define (make-list-of-objects number-of-objects constructor)
  (if (= number-of-objects 0)
      nil
      (cons (constructor) (make-list-of-objects (- number-of-objects 1) constructor))))


(define (make-semaphore n)
  ; mutexs is a list of mutexs
  (let ((mutexs (make-list-of-objects n make-mutex)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             
             mutexs)
            ((eq? m 'release)
             mutexs)))
    the-semaphore))

(define sema (make-semaphore 3))
(sema 'acquire)



