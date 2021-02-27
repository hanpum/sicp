(load-option 'format)
(load "12")

(define r1 (make-center-width 1 0.01))
(define r2 (make-center-width 3 0.03))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
		(add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
		  (add-interval (div-interval one r1)
				(div-interval one r2)))))

(define (par3 r1 r2)
  (let* ((cmp (lambda (v1 v2)
		(/ (* v1 v2)
		   (+ v1 v2))))
	 (lb1 (lower-bound r1))
	 (ub1 (upper-bound r1))
	 (lb2 (lower-bound r2))
	 (ub2 (upper-bound r2))
	 (v1 (cmp lb1 lb2))
	 (v2 (cmp lb1 ub2))
	 (v3 (cmp ub1 lb2))
	 (v4 (cmp ub1 ub2)))
    (make-interval (min v1 v2 v3 v4)
		   (max v1 v2 v3 v4))))


(display (format '#f "~%begin testing of 14-15.scm~%"))

(display (format '#f "~%R1: "))
(display-interval r1)
(display (format '#f "~%R2: "))
(display-interval r2)

(display (format '#f "~%R1*R2 / (R1+R2) = "))
(display-interval (par1 r1 r2))

(display (format '#f "~%1 / (1/R1 + 1/R2) = "))
(display-interval (par2 r1 r2))

(display (format '#f "~%correct value = "))
(display-interval (par3 r1 r2))
