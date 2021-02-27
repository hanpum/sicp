(define (prime? n)
  ;; is a dividable by b?
  (define (divides? a b)
    (= 0 (remainder a b)))

  ;; guess iterally 
  (define (guess-iter n guess)
    (cond ((> guess (square n)) n)
	  ((divides? n guess) guess)
	  (else (guess-iter n (+ 1 guess)))))

  ;; find smallest divisor for given number
  (define (smallest-divisor n)
    (guess-iter n 2))

  ;; if a number's smaller divisor it's itself, then it's prime
  (= n (smallest-divisor n)))



;; compute sqrt for given x
(define (sqrt x)
  (define (average x y)
    (/ (+ x y) 2.0))

  (define (good-enough guess)
    (< (abs (- x (* guess guess))) 0.0001))

  (define (improve guess)
    (average guess (/ x guess)))

  (define (sqrt-iter guess)
    (if (good-enough guess)
	guess
	(sqrt-iter (improve guess))))

  (sqrt-iter 1.0))
