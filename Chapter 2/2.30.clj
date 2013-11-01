;; Anything you type in here will be executed
;; immediately with the results shown on the
;; right.

(defn square-tree [n]
  (defn square [x] (* x x))
  (cond
   (not (list? n)) (square n)

   )


)


(square-tree (list 1 2 3))

