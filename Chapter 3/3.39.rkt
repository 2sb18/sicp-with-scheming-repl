(define x 10)

(define s (make-serializer))

(parallel-execute (lambda () 
                    (set! x 
                          ((s (lambda () (* x x))))))
                  (s (lambda () (set! x (+ x 1)))))

- 1st function has the getting of the variable serialized, the function carried out
  serialized, but not the setting of the variable. Let's call first part M and second
  part S.
- 2nd function has the whole thing serialized. It's incrementing x so we'll call it
  I
  
Possibilities: S has to come after M

MSI  x=10 internalx=100 x=100 x=101
MIS  x=10 internalx=100 x=11  x=100
IMS  x=10 x=11 internalx=121 x=121

  



