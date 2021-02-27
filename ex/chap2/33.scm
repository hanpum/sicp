(load "utils")

(define (my-map p seq)
  (accumulate (lambda (x rest)
		(append (list (p x))
			rest))
	      '()
	      seq))


;; should be result of (cons v1 (cons v2 (cons ...)))
(define (my-append seq1 seq2)
  (accumulate cons seq1 seq2))


(define (my-length seq)
  (accumulate (lambda (x rest)
		(+ rest 1))
	      0
	      seq))


(println "~%begin testing of 33")

(define data '(1 2 3 4 5))

(println "~S" (accumulate + 0 data))

(println "~S" (my-map square data))

(println "~S" (my-append data data))

(println "length of ~S: ~S" data (my-length data))
