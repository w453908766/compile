

(define (fact n)
  (if (= n 0) 1
  (* n (fact (- n 1)))))

(define (g) (fact 5))


(declare fact (-> Int Int))

(define n Int 5)

(define Int n 5)

(define (Int Int Int) (add a b) (+ a b))

(define (Int Int) (fact n)
  (if (= n 0) 1
  (* n (fact (- n 1)))))
  
