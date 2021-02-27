(define (double x)
  (+ x x))


(define (halve x)
  (/ x 2))


(define (l-fast* a b)
  (define (fast*-iter a b sum step)
    (cond ((= a 0) 0)
	  ((= b 0) sum)
	  ((odd? b) (fast*-iter a (- b 1) (+ sum step) step))
	  (else (fast*-iter a (halve b) sum (double step)))))
  (fast*-iter a b 0 a))

(l-fast* 101 101)
