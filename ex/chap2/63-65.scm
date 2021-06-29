(load "utils")

(define (element-of-tree? x tree)
  (cond ((null? tree) false)
	((= (tree-entry tree) x) true)
	((> (tree-entry tree) x)
	 (element-of-tree? x (tree-left-branch tree)))
	(else
	 (element-of-tree? x (tree-right-branch tree)))))


(define (adjoin-tree x tree)
  (if (null? tree) (make-tree x '() '())
      (let ((root (tree-entry tree))
	    (left (tree-left-branch tree))
	    (right (tree-right-branch tree)))
	(cond ((= x root) tree)
	      ((< x root) (make-tree root (adjoin-tree x left) right))
	      (else (make-tree root left (adjoin-tree x right)))))))


(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (tree-left-branch tree))
	      (cons (tree-entry tree)
		    (tree->list-1 (tree-right-branch tree))))))


(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
	result-list
	(copy-to-list (tree-left-branch tree)
		      (cons (tree-entry tree)
			    (copy-to-list (tree-right-branch tree)
					  result-list)))))
  (copy-to-list tree '()))


(define (list->tree-1 list)
  (define (iter list tree)
    (if (null? list) tree
	(iter (cdr list)
	      (adjoin-tree (car list) tree))))
    (iter list '()))


(define (list->tree-2 list)
  ;; build a partial tree from sorted elements
  (define (partial-tree elts n)
    (if (= n 0)
	(cons '() elts)
	(let ((left-size (quotient (- n 1) 2)))
	  (let* ((left-result (partial-tree elts left-size))
		 (left-tree (car left-result))
		 (non-left-elts (cdr left-result))
		 (right-size (- n left-size 1)))
	    (let* ((root (car non-left-elts))
		  (right-result (partial-tree (cdr non-left-elts) right-size))
		  (right-tree (car right-result))
		  (remaining-elts (cdr right-result)))
	      (cons (make-tree root left-tree right-tree)
		    remaining-elts))))))
  (car (partial-tree list (length list))))


(define (intersection-of-tree tree1 tree2)
  (define (iter list1 list2)
    (if (or (null? list1) (null? list2))
	'()
	(let ((e1 (car list1))
	      (e2 (car list2))
	      (rest1 (cdr list1))
	      (rest2 (cdr list2)))
	  (cond ((= e1 e2) (cons e1 (iter rest1 rest2)))
		((< e1 e2) (iter rest1 list2))
		((> e1 e2) (iter list1 rest2))))))
  (iter (tree->list-1 tree1)
	(tree->list-1 tree2)))


(define (union-of-tree tree1 tree2)
  (define (iter list1 list2)
    (cond ((null? list1) list2)
	  ((null? list2) list1)
	  (else (let ((e1 (car list1))
		      (e2 (car list2))
		      (rest1 (cdr list1))
		      (rest2 (cdr list2)))
		  (cond ((= e1 e2) (cons e1 (iter rest1 rest2)))
			((< e1 e2) (cons e1 (iter rest1 list2)))
			((> e1 e2) (cons e2 (iter list1 rest2))))))))
  (iter (tree->list-1 tree1)
	(tree->list-1 tree2)))



(println "~%begin testing of 63")



(define list1 '(1 2 3 4 5))
(define list2 '(2 4 6 8 10))

(define tree1 (list->tree-2 list1))
(define tree2 (list->tree-2 list2))


(println "~S~%list->tree-1: ~S~%list->tree-2: ~S~%"
	 list1
	 (list->tree-1 list1)
	 (list->tree-2 list1))


(println "~S~%tree->list-1: ~S~%tree->list-2: ~S" 
	 tree1
	 (tree->list-1 tree1)
	 (tree->list-1 tree1))


(println "intersection of ~S and ~S: ~S" tree1 tree2 (intersection-of-tree tree1 tree2))
(println "union of ~S and ~S: ~S" tree1 tree2 (union-of-tree tree1 tree2))
