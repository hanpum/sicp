(load "utils")

(define (subsets data)
  (if (null? data) (list '())
      (let ((rest (subsets (cdr data))))
	(append rest
		(map (lambda (item)
		       (append item
			       (list (car data))))
		     rest)))))

(println "~%begin testing of 32")

(define data '(1 2 3))

(println "subsets of ~S: ~S" data (subsets data))
