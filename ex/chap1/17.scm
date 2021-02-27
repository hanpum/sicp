(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))


(define (double x)
  (+ x x))


(define (halve x)
  (/ x 2))


(define (fast* a b)
  (cond ((= b 0) 0)
	((odd? b) (+ a (fast* a (- b 1))))
	(else (double (fast* a (halve b))))))


(fast* 5 20)
