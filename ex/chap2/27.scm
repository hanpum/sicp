(load "18")


(define (deep-reverse data)
  (define (iter x answer)
    (if (null? x) answer
	(iter (cdr x)
	      (cons (deep-reverse (car x))
		    answer))))
  (if (pair? data) (iter data '())
      data))


(println "~%begin testing of ~S" 27)

(define (test data)
  (println "deep-reverse of ~S: ~S" data (deep-reverse data)))

(test '())
(test '(1))
(test '(1 2 3 4))
(test '(1 2 3 4 (5 6 7)))
(test '((1 2) (3 4)))
(test '(1 2 (3 4) (5 6 7)))
