(load-option 'format)

;; compute fix point of given function, start from given guess value
(define (fixed-point f guess)
  (display (format '#f "~S\n" guess))
  (define tolerance 0.0001)
  (define next-val (f guess))
  (define (good-enough? val)
    (> tolerance
       (abs (- val guess))))
  (if (good-enough? next-val)
      guess
      (fixed-point f next-val)))


;; y^2 = x  ==>  y = x/y  ==>  2y = y + x/y  ==>  y = (y+x/y)/2
(define (sqrt x)
  (fixed-point (lambda (y)
		 (/ (+ y (/ x y)) 2.0))
	       1.0))


;; agveage damp of given function, namely, [x + f(x)]/2
(define (average-damp f)
  (lambda (x)
    (/ (+ (f x) x) 2)))


(define (x^x x)
  (fixed-point (lambda (y)
		 (/ (log x) (log y)))
	       2.0))


(define (x^x-with-damp x)
  (fixed-point (average-damp
		(lambda (y)
		  (/ (log x) (log y))))
	       2.0))


;;(sqrt 9)

(x^x 1000)
(newline)

(x^x-with-damp 1000)
