(load "utils")

(define (count-leaves tree)
  (cond ((null? tree) 0)
	((not (pair? tree)) 1)
	(else (+ (count-leaves (car tree))
		 (count-leaves (cdr tree))))))


;; count tree leaves with accumulate
(define (count-leaves2 tree)
  (accumulate + 0 (map (lambda (sub-tree)
			 (cond ((null? sub-tree) 0)
			       ((not-pair? sub-tree) 1)
			       (else (count-leaves2 sub-tree))))
			 tree)))


(println "~%begin testing of 35")

(define tree (list '(1 2) (list 3 4) 5 6))

(println "count-leaves of ~S: ~S" tree (count-leaves tree))
(println "count-leaves2 of ~S: ~S" tree (count-leaves2 tree))
