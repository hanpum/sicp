;; compute power of given number, using iter-compute style

;;; denote n as 01 binary code
(define (fast-exp b n)
  (define (fast-exp-iter b n a m)
    (cond ((= n 0) a)
	  ((odd? n) (fast-exp-iter b (- n 1) (* a m) m))
	  (else (fast-exp-iter b (/ n 2) a (* m m)))))
  (fast-exp-iter b n 1.0 b))

(define (test-iter b n)
  (cond ((= n 0) #t)
	(else (begin
		(write (fast-exp b n))
		(newline)
		(test-iter b (- n 1))))))

(test-iter 3 25)
