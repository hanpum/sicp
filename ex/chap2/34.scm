(load "utils")


(define (horner-eval x coefficient-seq)
  (accumulate (lambda (this-coeff higher-terms)
		(+ this-coeff
		   (* x higher-terms)))
	      0
	      coefficient-seq))


(println "~%begin testing of 34")

(define coeff '(1 3 0 5 0 1))
(println "~S" (horner-eval 2 coeff))
