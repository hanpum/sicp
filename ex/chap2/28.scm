(load "utils")

(define (fringe data)
  (cond ((null? data) '())
	((pair? data) (append (fringe (car data))
			      (fringe (cdr data))))
	(else (list data))))


(println "~%begin testing of 28")


(define (test data)
  (println "~S: ~S" data (fringe data)))

(define x (list (list 1 2)
		(list 3 4)))


(test (list (list 1 2)
	    3
	    4))

(test x)
(test (list x x))
	
