(define x 10)

(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (* x x x))))

- you got 1A : when first thing accesses
          1S : when 1 sets
          2A : when 2 accesses
          2S : when 2 sets
          
1A 1S 2A 2S  100 1000000
1A 2A 1S 2S  1000
1A 2A 2S 1S  100

2A 2S 1A 1S  1000 1000000
2A 1A 2S 1S  100
2A 1A 1S 2S  1000

100, 1000, or 1000000.
It could be that the accesses can also be interleaved, in which case shit really
get's fucked!


(define x 10)

(define s (make-serializer))

(parallel-execute (s (lambda () (set! x (* x x))))
                  (s (lambda () (set! x (* x x x)))))

now two possibilities: 1st runs before 2nd: 10 100 1000000
                       2nd runs before 1st: 10 1000 1000000
                       both have same result, since (a^b)^c = a^(b*c)
                       
                       
  



