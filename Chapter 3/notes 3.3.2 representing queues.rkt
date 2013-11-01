; queue operations
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

; they did it almost like this, except they used '() instead
; of nil
(define (make-queue)
  (cons nil nil))

; they did it exactly like this
(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "front-queue called on an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((item-cons (cons item nil)))
        (if (empty-queue? queue)
            (begin (set-front-ptr! queue item-cons)
                   (set-rear-ptr! queue item-cons)
                   queue)
            (begin (set-cdr! (rear-ptr queue) item-cons)
                   (set-rear-ptr! queue item-cons)
                   queue))))

(define (delete-queue! queue)
  (if (empty-queue? queue)
      (error "delete-queue! called on an empty queue" queue)
      (begin (set-front-ptr! queue (cdr (front-ptr queue)))
             queue)))

(define q (make-queue))	
(insert-queue! q 'a)   ;	a
(insert-queue! q 'b)   ;	a b
(delete-queue! q)      ;	b
(insert-queue! q 'c)   ;	b c
(insert-queue! q 'd)   ;    b c d
(delete-queue! q)      ;	c d

   
