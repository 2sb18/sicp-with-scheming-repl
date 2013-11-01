(define (make-account balance password)
  (let ((incorrect-tries 0))
    (define (call-the-cops)
      "The cops have been called motherfucker!")
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (lambda (password-guess m)
      (if (not (eq? password-guess password))
          (lambda (x)
            (set! incorrect-tries (+ incorrect-tries 1))
            (if (> incorrect-tries 7)
                (call-the-cops)
                "Incorrect password"))
          (begin (set! incorrect-tries 0)
                 (cond ((eq? m 'withdraw) withdraw)
                       ((eq? m 'deposit) deposit)
                       ((eq? m 'try-password) #t)
                       (else (error "Unknown request -- MAKE-ACCOUNT"  m))))))))



(define (make-joint account old-password new-password)
  (if (eq? "Incorrect password"
           ((account old-password 'withdraw) 0))
      (error "Can't make joint account because password is incorrect")
      (lambda (password-guess m)
        (if (not (eq? password-guess new-password))
            "Incorrect password for joint account"
            (account old-password m)))))
         
(define peter-acc (make-account 100 'open-sesame))
(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))

((paul-acc 'rosebud 'withdraw) 10)
((peter-acc 'open-sesame 'withdraw) 13)
  