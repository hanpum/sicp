(load-option 'format)


(define (do-make-rat x y)
  (let ((g (gcd x y)))
    (cons (/ x g)
	  (/ y g))))


(define (make-rat x y)
  (let ((prod (* x y))
	(ax (abs x))
	(ay (abs y)))
    (cond ((= y 0) (display "denom should't be zero"))
	  ((< prod 0) (do-make-rat (* -1 ax) ay))
	  (else (do-make-rat ax ay)))))


(define (numer x) (car x))
(define (denom x) (cdr x))


;; a/b + c/d = (ad + bc)/db
(define (add-rat a b)
  (make-rat (+ (* (numer a) (denom b))
	       (* (numer b) (denom a)))
	    (* (denom a) (denom b))))


;; a/b - c/d = (ad - bc)/db
(define (sub-rat a b)
  (make-rat (- (* (numer a) (denom b))
	       (* (numer b) (denom a)))
	    (* (denom a) (denom b))))


;; a/b * c/d = (a*c)/(b*d)
(define (mul-rat a b)
  (make-rat
   (* (numer a) (numer b))
   (* (denom a) (denom b))))


;; a/b / c/d = (a*d)/(b*c)
(define (div-rat a b)
  (make-rat (* (numer a) (denom b))
	    (* (denom a) (numer b))))


;; a/b == c/d  ==>  ad = bc
(define (equal-rat? a b)
  (= (* (numer a) (denom b))
     (* (denom a) (numer b))))


;; display a rational number
(define (print-rat x)
  (display (format '#f "~S/~S = " (numer x) (denom x)))
  (cond ((= 0 (numer x)) (display "0\n"))
	((= 1 (denom x)) (display (format
				   '#f "~S\n"
				   (numer x))))
	(else (display (format
			'#f "~S/~S\n"
			(numer x) (denom x))))))

(newline)


(print-rat (make-rat 2 -2))
(print-rat (make-rat 0 -2))


;; test previous definition
(let ((a (make-rat 1 2))
      (b (make-rat 3 5)))
  (display "a = ")
  (print-rat a)

  (display "b = ")
  (print-rat b)

  (display "a + b = ")
  (print-rat (add-rat a b))

  (display "a - b = ")
  (print-rat (sub-rat a b))

  (display "a * b = ")
  (print-rat (mul-rat a b))

  (display "a / b = ")
  (print-rat (div-rat a b)))
