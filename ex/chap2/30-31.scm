(load "utils")


(define (square-tree tree)
  (cond ((null? tree) '())
	((pair? tree) (cons (square-tree (car tree))
			    (square-tree (cdr tree))))
	(else (* tree tree))))


(define (square-tree-with-map tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (square-tree-with-map sub-tree)
	     (* sub-tree sub-tree)))
       tree))


(define (tree-map op tree)
  (cond ((null? tree) '())
	((pair? tree) (cons (tree-map op (car tree))
			    (tree-map op (cdr tree))))
	(else (op tree))))


(println "~%begin testing of 30-31")


(define data (list 1
		   (list 2
			 (list 3 4)
			 5)))

(println "squre-tree of ~S: ~S" data (square-tree data))
(println "square-tree-with-map of ~S: ~S" data (square-tree-with-map data))
(println "tree-map-of-square of ~S: ~S" data (tree-map square data))
