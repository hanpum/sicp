(load "utils")
(load-option 'format)


(define (reverse data)
  (define (iter x answer)
    (if (null? x)
	answer
	(iter (cdr x)
	      (cons (car x) answer))))
  (iter data '()))


(println "~%begin testing of 18.scm")


(define (test data)
  (println "reverse of ~S: ~S" data (reverse data)))

(test '())
(test '(1))
(test '(1 2 3 4))
