(load "utils")

(define (element-of-set? x set)
  (cond ((null? set) false)
	((= x (car set)) true)
	((< x (car set)) false)
	(else (element-of-set? x (cdr set)))))


(define (adjoin-set x set)
  (define (iter x set buf)
    (cond ((null? set)
	   (append buf '(x)))
	  ((< x (car set))
	   (append buf (list x) set))
	  (else
	   (iter x
		 (cdr set)
		 (append buf (list (car set)))))))
  (if (element-of-set? x set)
      set
      (iter x set '())))


(define (intersection-of-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
	((= (car set1) (car set2))
	 (cons (car set1)
	       (intersection-of-set (cdr set1)
				    (cdr set2))))
	((< (car set1) (car set2))
	 (intersection-of-set (cdr set1) set2))
	(else
	 (intersection-of-set set1 (cdr set2)))))


(define (union-of-set set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	((= (car set1) (car set2))
	 (cons (car set1)
	       (union-of-set (cdr set1)
			     (cdr set2))))
	((< (car set1) (car set2))
	 (cons (car set1)
	       (union-of-set (cdr set1) set2)))
	(else
	 (cons (car set2)
	       (union-of-set set1 (cdr set2))))))


(println "~%begin testing of 61")

(define set1 '(1 2 3 4 5))
(define set2 '(2 4 6 8 10))


(println "adjoin of 7 into ~S: ~S" set2 (adjoin-set 7 set2))
(println "intersection of ~S and ~S: ~S" set1 set2 (intersection-of-set set1 set2))
(println "union of ~S and ~S: ~S" set1 set2 (union-of-set set1 set2))
