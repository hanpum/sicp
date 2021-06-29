(load "utils")


;; make tri pairs composite of (i,j,k), for which bot 1<=i,j,k<=n
(define (make-tri-pairs n)
  (accumulate append '()
	      (map (lambda (i)
		     (accumulate append '()
				 (map (lambda (j)
					(map (lambda (k) (list i j k))
					     (enumerate-integers 1 n)))
				      (enumerate-integers 1 n))))
		   (enumerate-integers 1 n))))


;; get tri pairs that all element are not equal and sum of all elements equal to m
(define (uniq-trisum-pairs n m)
  (filter (lambda (item)
	    (let ((i (car item))
		  (j (cadr item))
		  (k (caddr item)))
	      (and (not (= i j))
		   (not (= i k))
		   (not (= j k))
		   (= (+ i j k) m))))
	  (make-tri-pairs n)))


;;; given a tri pairs, append it's sum of all elements as last element
(define (make-pair-sum pair)
  (let ((i (car pair))
	(j (cadr pair))
	(k (caddr pair)))
    (list i j k (+ i j k))))


(define (trisum-pairs n m)
  (map make-pair-sum 
       (uniq-trisum-pairs n m)))


(println "~%begin testing of 41.scm")

(println-list (trisum-pairs 10 10))
