(load-option 'format)

;; why return a function of funtion?
;; (zero a) -> ((lambda (f) (lambda (x) x)) a) -> (lambda (x) x)
(define zero (lambda (f)
	       (lambda (x)
		 x)))

;; (add-1 a) -> (lambda (f) (lambda (x) (f (n f) x)) a)
;;           -> (lambda (f) (lambda (x) (f (a f) x)))
(define (add-1 n)
  (lambda (f)
    (lambda (x)
      (f (n f) x))))

;; (add-1 zero) -> (lambda (f) (lambda (x) (f (zero f) x)))
;;              -> (lambda (f) (lambda (x) (f (lambda (y) y) x)))
;;
;; (one f) -> (lambda (x) (f (lambda (y) y) x))
(define one
  (lambda (f)
    (lambda (x)
      (f (lambda (y) y)
       x))))


;; (add-1 one) -> (lambda (f) (lambda (x) (f (one f) x)))
;;             -> (lambda (f) (lambda (x) (f (lambda (z) (f (lambda (y) y) z)) x))) 
(define two
  (lambda (f)
    (lambda (x)
      (f (lambda (y)
	   (f (lambda (z) z)
	      y))
	 x))))



((add-1 zero) zero)
