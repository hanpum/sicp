(load-option 'format)
(load "utils")

(define (for-each proc data)
  (if (not (null? data))
      (begin (proc (car data))
	     (for-each proc (cdr data)))))


(println "~%begin testing of 23")
(for-each (lambda (x)
	    (display x)
	    (newline))
	  '(1 2 3 4 5))
