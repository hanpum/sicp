(load "utils")

(define (reverse-right seq)
  (fold-right (lambda (x rest)
		(append rest (list x)))
	      '()
	      seq)
  )

(define (reverse-left seq)
  (fold-left (lambda (result x)
	       (append (list x) result))
	     '()
	     seq))


(println "~%begin testing of 39")


(define (test seq)
  (println "reverse-right of ~S: ~S" seq (reverse-right seq))
  (println "reverse-left of ~S: ~S" seq (reverse-left seq))
  (newline))


(test '())
(test '(1))
(test '(1 2 3))
(test '(1 (2 3) 4 5 6))
