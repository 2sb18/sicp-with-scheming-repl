(define (test description expression-to-run expected-result)
  (display description) (display ": ")
  (cond ((not (equal? (eval expression-to-run (interaction-environment)) expected-result))
         (display "failed\n")
         (display "Ran: ") (display expression-to-run) (newline)
         (display "Expected: ") (display expected-result) (newline)
         (display "Instead got: ") (display (eval expression-to-run (interaction-environment)))
         (newline))
        (else (display "passed\n"))))

(add-poly '(x (1 1) (0 2)) '(y (1 1) (0 3)))

; general stuff
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  (error "could not find procedure -- GET" key-1 key-2)))
            (error "could not find procedure -- GET" key-1 key-2))))
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

(define (raise-lower-to-higher lower higher)
  (cond ((not lower) #f)
        ((eq? (type-tag lower) (type-tag higher)) lower)
        (else (raise-lower-to-higher (raise lower) higher))))

(define (drop x)
  (let ((projected-x (project x)))
    (if (not projected-x)
        x
        (let ((raised-projected-x (raise projected-x)))
          (if (equ? x raised-projected-x)
              (drop projected-x)
              x)))))

(define (apply-generic operator x . args)
  (cond ((= (length args) 0)
         ((get operator (list (type-tag x))) (contents x)))
        ((= (length args) 1)
         (let ((x-raised (raise-lower-to-higher x (car args)))
               (y-raised (raise-lower-to-higher (car args) x)))
           (cond (x-raised
                  ((get operator (list (type-tag x-raised) (type-tag (car args)))) (contents x-raised)
                                                                                   (contents (car args))))
                 (y-raised
                  ((get operator (list (type-tag x) (type-tag y-raised))) (contents x)
                                                                          (contents y-raised)))
                 (else (error "can't raise one argument to the other -- APPLY-GENERIC"
                              x args)))))
        (else (error "too many arguments -- APPLY-GENERIC" args))))


(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))
(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum -- CONTENTS" datum))))

; generic procedures
(define (add x y) (drop (apply-generic 'add x y)))
(define (sub x y) (drop (apply-generic 'sub x y)))
(define (mul x y) (drop (apply-generic 'mul x y)))
(define (div x y) (apply-generic 'div x y))
(define (square x) (drop (apply-generic 'square x)))
(define (sqroot x) (drop (apply-generic 'sqroot x)))
(define (sine x) (drop (apply-generic 'sine x)))
(define (cosine x) (drop (apply-generic 'cosine x)))
(define (arctan x y) (drop (apply-generic 'arctan x y)))

(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (raise x) (apply-generic 'raise x))
(define (project x) (apply-generic 'project x))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

; scheme-number
(define (install-scheme-number-package)  
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (+ x y)))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (- x y)))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (* x y)))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (/ x y)))
  (put 'square '(scheme-number)
       (lambda (x) (* x x)))
  (put 'sqroot '(scheme-number)
       (lambda (x) (sqrt x)))
  (put 'sine '(scheme-number)
       (lambda (x) (sin x)))
  (put 'cosine '(scheme-number)
       (lambda (x) (cos x)))
  (put 'arctan '(scheme-number scheme-number)
       (lambda (x y) (atan x y)))
  (put 'make '(scheme-number)
       (lambda (x) x))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  (put 'raise '(scheme-number)
       (lambda (x) (make-rational x 1)))
  (put 'project '(scheme-number)
       (lambda (x) #f))    ; can't project an ordinary number
  'done)
(install-scheme-number-package)
(define (make-scheme-number n)
  ((get 'make '(scheme-number)) n))

; rational
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((num (/ n d)))
      (cons (inexact->exact (numerator num))
            (inexact->exact (denominator num)))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'square '(rational)
       (lambda (x) (tag (make-rat (* (numer x) (numer x)) (* (denom x) (denom x))))))
  (put 'sqroot '(rational)
       (lambda (x) (tag (make-rat (sqrt (numer x)) (sqrt (denom x))))))
  (put 'sine '(rational)
       (lambda (x) (tag (make-rat (sin (/ (numer x) (denom x))) 1))))
  (put 'cosine '(rational)
       (lambda (x) (tag (make-rat (cos (/ (numer x) (denom x))) 1))))
  (put 'arctan '(rational)
       (lambda (x) (tag (make-rat (atan (numer x) (denom x))))))
  (put 'equ? '(rational rational)
       (lambda (x y) (= (/ (numer x) (denom x)) (/ (numer y) (denom y)))))
  (put '=zero? '(rational)
       (lambda (x) (= (numer x) 0)))
  (put 'raise '(rational)
       (lambda (x) (make-complex-from-real-imag (div (numer x) (denom x)) 0)))
  (put 'project '(rational)
       (lambda (x) (make-scheme-number (round (/ (numer x) (denom x))))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)
(install-rational-package)
(define (make-rational n d)
  ((get 'make 'rational) n d))

; complex
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqroot (add (square (real-part z))
                 (square (imag-part z)))))
  (define (angle z)
    (arctan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (cons (mul r (cosine a)) (mul r (sine a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)
(install-rectangular-package)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (mul (magnitude z) (cosine (angle z))))
  (define (imag-part z)
    (mul (magnitude z) (sine (angle z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt (add (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)
(install-polar-package)

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (add (real-part z1) (real-part z2))
                         (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2))
                         (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                       (add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                       (sub (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'equ? '(complex complex)
       (lambda (z1 z2) (and (= (real-part z1) (real-part z2))
                            (= (imag-part z1) (imag-part z2)))))
  (put '=zero? '(complex)
       (lambda (z) (= (magnitude z) 0)))
  (put 'raise '(complex)   ; kinda weird, but we're raising a complex number to a
       (lambda (z) (make-polynomial 'x (list (list 0 (tag z))))))    ; polynomial, so we can do things like add polynomials
  ; and other numbers
  (put 'project '(complex)
       (lambda (x) (make-rational (inexact->exact (numerator (real-part x)))
                                  (inexact->exact (denominator (real-part x))))))
  'done)
(install-complex-package)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;polynomial
;(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable (clean-terms term-list)))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x)
    (symbol? x))
  (define (same-variable? x y)
    (and (variable? x) (variable? y) (eq? x y)))
  
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           ; first-term returns the highest-order term
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                ; since we're using the generic add operation,
                                ; the coeff can be any type!
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))
  ; returns a list with two things, first the quotient term-list, then the remained term-list
  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))     ; 
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1)
              (let ((new-coeff (div (coeff t1) (coeff t2)))
                    (new-order (- (order t1) (order t2))))
                ;(add-terms L1 (neg-terms (mul-term-by-all-terms (make-term new-order new-coeff) L2))))))))
                (let ((rest-of-result
                       (div-terms (add-terms L1 (neg-terms (mul-term-by-all-terms (make-term new-order new-coeff) L2)))
                                  L2)
                       ))
                  (list (adjoin-term (make-term new-order new-coeff) (car rest-of-result)) (cadr rest-of-result))))))))
  
  ;; representation of terms and term lists
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  ; runs the term-list through adjoin-term to get rid
  ; of terms with zero coefficients
  ; also, drop the coefficients down to the simplest
  ; form
  (define (clean-terms term-list)
    (if (empty-termlist? term-list)
        '()
        (let ((term (first-term term-list)))
          ;(adjoin-term (make-term (order term) (drop (coeff term))) (rest-terms term-list)))))
          (adjoin-term (make-term (order term) (coeff term)) (rest-terms term-list)))))
  (define (neg-terms term-list)
    (define (neg-term term)
      (make-term (order term) (sub 0 (coeff term))))
    (map neg-term term-list))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (=zero-poly? termlist)
    (cond ((empty-termlist? termlist) #t)
          ((=zero? (coeff (first-term termlist))) (=zero-poly? (rest-terms termlist)))
          (else #f)))
  (define (term-lists-equ? tl1 tl2)
    (cond ((null? tl1) #t)
          ((and (= (order (car tl1)) (order (car tl2)))
                (equ? (coeff (car tl1)) (coeff (car tl2)))) (term-lists-equ? (cdr tl1) (cdr tl2)))
          (else #f)))
  ; turn the poly into the zero coefficient of the new variable poly
  ; so (change-var-of-poly 'x '(polynomial y ((1 1) (0 3))))
  (define (change-var-of-poly var p)
    (make-poly var (list (make-term 0 p))))
    
  (define (add-poly p1 p2)
    (let ((var (get-correct-var p1 p2)))
      (if var
          (make-poly var (add-terms (term-list p1) (term-list p2)))
          ; change the variable of the second polynomial into the variable of the first
          (let ((new-poly (change-var-of-poly (variable p1) p2)))
            (make-poly (variable p1) (add-terms (term-list p1) (term-list p2)))))))
         ; (error "Polynomials not in same var -- ADD-POLY" (list p1 p2)))))
  (define (neg-poly p)
    (make-poly (variable p) (neg-terms (term-list p))))
  (define (sub-poly p1 p2)
    (add-poly p1 (neg-poly p2)))
  (define (mul-poly p1 p2)
    (let ((var (get-correct-var p1 p2)))
      (if var
          (make-poly var (mul-terms (term-list p1) (term-list p2))))))
  (define (get-correct-var p1 p2)
    (cond ((same-variable? (variable p1) (variable p2)) (variable p1))
          ((= (order (first-term (term-list p1))) 0) (variable p2))
          ((= (order (first-term (term-list p2))) 0) (variable p1))
          (else #f)))
  (define (div-poly p1 p2)
    ;(div-terms (term-list p1) (term-list p2)))
  
    (let ((result (div-terms (term-list p1) (term-list p2)))
          (var (cond ((same-variable? (variable p1) (variable p2)) (variable p1))
                     ((= (order (first-term (term-list p1))) 0)    (variable p2))
                     ((= (order (first-term (term-list p2))) 0)    (variable p1))
                     (else (error "Polynomials not in same var -- DIV-POLY" (list p1 p2))))))
      (list (tag (make-poly var (car result))) (drop (tag (make-poly var (cadr result)))))))
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial) 
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2) (div-poly p1 p2)))
  (put '=zero? '(polynomial) 
       (lambda (p) (=zero-poly? (term-list p))))
  (put 'equ? '(polynomial polynomial)
       (lambda (p1 p2)
         (cond ((not (eq? (variable p1) (variable p2))) #f)
               ((not (= (length (term-list p1)) (length (term-list p2)))) #f)
               (else (term-lists-equ? (term-list p1) (term-list p2))))))
  (put 'raise '(polynomial)
       (lambda (p) #f))
  (put 'project '(polynomial)  ; bring down to complex number. using shitty way, highest 
       (lambda (p) (make-complex-from-real-imag 
                    (if (empty-termlist? (term-list p))
                        0
                        (coeff (first-term (term-list p)))) 
                    0)))              
  ; order coefficient becomes complex number
  (put 'make '(polynomial)
       (lambda (var terms) (tag (make-poly var terms))))
;  'done)
;(install-polynomial-package)
(define (make-polynomial var terms)
  ((get 'make '(polynomial)) var terms))

;; tests

#|

(define z1 (make-polynomial 'x '((1 1) (0 4))))
(test "adding 0 to a polynomial" '(add z1 0) '(polynomial x (1 1) (0 4)))

(define x (make-scheme-number 3))
(define z26 (make-complex-from-real-imag 3 0))
(test "checking equality of number and complex" '(equ? x z26) #t)

(define p34 (make-polynomial 'x '((0 3))))
(test "checking equality of number and polynomial" '(equ? x p34) #t)

(test "checking equality of complex and polynomial" '(equ? z26 p34) #t)

(define zero-poly (make-polynomial 'x '()))
(test "dropping a zero polynomial" '(drop zero-poly) 0)

(define z2 (make-polynomial 'x '((1 3))))
(define z3 (make-polynomial 'x '((1 -3))))
(test "adding together polynomials that should result in zero" '(add z2 z3) 0)

(define z49 (make-polynomial 'x '((4 3) (3 2) (1 0) (0 6))))
(test "subtracting two polynomials that should result in zero" '(sub z49 z49) 0)

(define p94 (make-polynomial 'x '((5 1) (0 -1))))
(define p92 (make-polynomial 'x '((2 1) (0 -1))))
(test "dividing two polynomials" '(div p94 p92) '((polynomial x (3 1) (1 1)) (polynomial x (1 1) (0 -1))))

(define p59 (make-polynomial 'x '((2 1) (0 1))))
(define r34 (make-rational 4 5))
(test "dividing a poly by a rational" '(div p59 r34) '(((2 (complex polar 5/4 . 0)) (0 (complex polar 5/4 . 0))) ()))

(define p93 (make-polynomial 'x '((2 1) (0 1))))
(define p69 (make-polynomial 'x '((0 (rational 4 . 5)))))
(test "diving a polynomial by a polynomial with rational coefficients" '(div p93 p69) 
      '((polynomial x ((1 1) (0 (complex rectangular 0 -1)))) (polynomial x)))

(define p29 (make-polynomial 'x '((1 1) (0 2))))
(define p49 (make-polynomial 'y '((1 1) (0 3))))

;(add p29 p49)

|#

















































