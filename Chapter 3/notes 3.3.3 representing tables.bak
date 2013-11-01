(define (make-deque)
  (cons nil nil))

(define (empty-deque? deque)
  (null? (car deque)))

(define (print-deque deque)
  (define (iter deque-left)
    (if (not (null? deque-left))
        (begin (display (car deque-left))
               (display " ")
               (iter (cadr deque-left)))))
  (display "( ")
  (iter (car deque))
  (display ")\n"))

(define (print-deque-backwards deque)
  (define (iter deque-left)
    (if (not (null? deque-left))
        (begin (display (car deque-left))
               (display " ")
               (iter (cddr deque-left)))))
  (display " backwards ( ")
  (iter (cdr deque))
  (display ")\n"))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "front-deque called on an empty deque" deque)
      (caar deque)))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "rear-deque called on an empty deque" deque)
      (cadr deque)))

(define (front-insert-deque! deque item)
  (let ((item-list (cons item (cons (car deque) nil))))
    (if (empty-deque? deque)
        (begin (set-car! deque item-list)
               (set-cdr! deque item-list))
        (begin (set-cdr! (cdar deque) item-list)
               (set-car! deque item-list)))))

(define (rear-insert-deque! deque item)
  (let ((item-list (cons item (cons nil (cdr deque)))))
    (if (empty-deque? deque)
        (begin (set-car! deque item-list)
               (set-cdr! deque item-list))
        (begin (set-car! (cddr deque) item-list)
               (set-cdr! deque item-list)))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "front-delete-deque! called on empty deque" deque))
        ((eq? (car deque) (cdr deque))
         (set-car! deque nil)
         (set-cdr! deque nil))
        (else (set-car! deque (cadar deque))
              (set-cdr! (cdar deque) nil))))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "rear-delete-deque! called on empty deque" deque))
        ((eq? (car deque) (cdr deque))
         (set-car! deque nil)
         (set-cdr! deque nil))
        (else (set-cdr! deque (cdddr deque))
              (set-car! (cddr deque) nil))))

(define q1 (make-deque))
q1
(front-insert-deque! q1 'a)
q1
(print-deque q1) ; a
(print-deque-backwards q1)
(front-insert-deque! q1 'b)
(print-deque q1) ; b a
(print-deque-backwards q1)
(rear-insert-deque! q1 'c)
(print-deque q1) ; b a c
(print-deque-backwards q1)
(front-insert-deque! q1 'd)
(print-deque q1) ; d b a c
(print-deque-backwards q1)






(front-delete-deque! q1)
(print-deque q1) ; b a c
(print-deque-backwards q1)
;(rear-delete-deque! q1)
;(print-deque q1) ; b a
;(rear-delete-deque! q1)
;(print-deque q1) ; b
;(rear-delete-deque! q1)
;(print-deque q1) ; 

(define q1 (make-deque))
(rear-insert-deque! q1 'z)
(print-deque q1) ; z
(print-deque-backwards q1)
(rear-insert-deque! q1 'x)
(print-deque q1) ; z x
(print-deque-backwards q1)
(front-insert-deque! q1 'y)
(print-deque q1) ; y z x
(print-deque-backwards q1)
(front-deque q1)
(rear-deque q1)
(front-delete-deque! q1)
(print-deque q1)
(print-deque-backwards q1)
(rear-delete-deque! q1)
(print-deque q1)
(print-deque-backwards q1)





   
