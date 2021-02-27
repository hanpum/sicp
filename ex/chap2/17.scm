(load-option 'format)

(define (last-pair data)
  (define (iter x answer)
    (if (null? x)
	answer
	(iter (cdr x) (car x))))
  (iter data '()))

(display (format '#f "~%begin testing of 17.scm~%"))

(define (test data)
  (display (format '#f "last pair of ~S is: ~S~%" data (last-pair data))))

(test '())
(test (list 1))
(test (list 1 2 3 4 5))
