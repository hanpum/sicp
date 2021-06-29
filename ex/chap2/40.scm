(load "utils")


;; make pair of (i,j) for which 1<=j<i<=n
(define (unique-pairs n)
  (flate-map append 
	   (map (lambda (i)
		  (map (lambda (j) (list i j))
		       (enumerate-integers 1 (- i 1))))
		(enumerate-integers 2 n))))


(define (make-pair-sum pair)
  (list (car pair)
	(cadr pair)
	(+ (car pair)
	   (cadr pair))))


(define (primer-sum? pair)
  (prime? (+ (car pair) (cadr pair))))


(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter primer-sum?
	       (unique-pairs n))))


(println "~%begin testing of 40.scm")
(println-list (prime-sum-pairs 10))
