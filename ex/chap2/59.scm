(load "utils")

(define (element-of-set? x set)
  (cond ((null? set) false)
	((equal? x (car set)) true)
	(else (element-of-set? x (cdr set)))))


;; add x to given set
(define (adjoin-set x set)
  (if (element-of-set? x set) set
      (cons x set)))


(define (intersection-of-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
	((element-of-set? (car set1) set2)
	 (cons (car set1)
	       (intersection-of-set (cdr set1) set2)))
	(else (intersection-of-set (cdr set1) set2))))


(define (union-of-set set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	((element-of-set? (car set1) set2)
	 (union-of-set (cdr set1) set2))
	(else (cons (car set1)
		    (union-of-set (cdr set1) set2)))))


(println "~%begin testing of 59")


(define set1 '(1 2 3 4 5))
(define set2 '(2 4 6 8 10))


(println "intersection of ~S and ~S: ~S" set1 set2 (intersection-of-set set1 set2))
(println "union of ~S and ~S: ~S" set1 set2 (union-of-set set1 set2))
