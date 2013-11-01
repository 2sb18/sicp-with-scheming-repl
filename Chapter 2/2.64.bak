#lang planet neil/sicp

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

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set) 
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

(define (create-set list)
  (define (loop list result)
    (if (null? list)
        result
        (loop (cdr list) (adjoin-set (car list) result))))
  (loop list '()))


; works as advertised I think
; every node of the tree has to have a tree->list-1 function
; called, so it's O(n) 
; O(n) I think
; actually no, from Bill the Lizard it states that append is O(n), 
;
; we're looking for the amount of time that append takes.
; there's an append for each node. Then you multiply that by the
; time the append takes. When you double n, time that append takes
; goes up by a constant amount, so it's O(n*log n)
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

; I think this takes the same amount of time O(n)
; it's kinda half iterative
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))


(define l1 (create-set '(7 3 9 1 5 11)))
(define l2 (create-set '(3 1 7 5 9 11)))
(define l3 (create-set '(5 3 9 1 7 11)))
(pl (tree->list-1 l1))
(pl (tree->list-2 l1))
(pl (tree->list-1 l2))
(pl (tree->list-2 l2))
(pl (tree->list-1 l3))
(pl (tree->list-2 l3))






      












       