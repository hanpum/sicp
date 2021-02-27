(load-option 'format)

;; ex 2.7
(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))


(define (add-interval x y)
  (make-interval
   (+ (lower-bound x) (lower-bound y))
   (+ (upper-bound x) (upper-bound y))))


;; ex 2.8
(define (sub-interval x y)
  (add-interval x
   (* -1.0 (upper-bound y))
   (* -1.0 (lower-bound y))))


(define (mul-interval x y)
  (let* ((lbx (lower-bound x))
	 (ubx (upper-bound x))
	 (lby (lower-bound y))
	 (uby (upper-bound y))
	 (p1 (* lbx lby))
	 (p2 (* lbx uby))
	 (p3 (* ubx lby))
	 (p4 (* ubx uby)))
    (make-interval
     (min p1 p2 p3 p4)
     (max p1 p2 p3 p4))))

;; 2-11
(define (mul-interval-opt x y)
  (let* ((lbx (lower-bound x))
	 (ubx (upper-bound x))
	 (lby (lower-bound y))
	 (uby (upper-bound y)))
    (cond
     ((<= ubx 0) (cond
		  ((<= uby 0) (make-interval
			       (* ubx uby)
			       (* lbx lby)))
		  ((and (> uby 0) (< lby 0)) (make-interval
					      (* ubx uby)
					      (* ubx uby)))
		  ((>= lby 0) (make-interval
			       (* lbx uby)
			       (* lbx lby)))))
     ((and (> ubx 0) (< lbx 0)) (cond
				 ((<= uby 0) (make-interval
					      (* ubx lby)
					      (* lbx lby)))
				 ((and (> uby 0) (< lby 0)) (make-interval
							     (min (* lbx uby) (* ubx lby))
							     (max (* lbx lby) (* ubx uby))))
				 ((>= lby 0) (make-interval
					      (* lbx uby)
					      (* ubx uby)))))
     ((> lbx 0) (cond
		 ((<= uby 0) (make-interval
			      (* ubx lby)
			      (* lbx uby)))
		 ((and (> uby 0) (< lby 0)) (make-interval
					     (* ubx lby)
					     (* ubx uby)))
		 ((>= lby 0) (make-interval
			      (* lbx lby)
			      (* ubx uby))))))))


(define (div-interval x y)
  (let ((lby (lower-bound y))
	(uby (upper-bound y)))
    (if (or (= 0 lby) (= 0 uby))
	(error "can't div interval that bound on 0")
	(mul-interval x
		      (make-interval
		       (/ 1.0 uby)
		       (/ 1.0 lby))))))


(define (display-interval x)
  (display (format '#f "[~S, ~S]"
		   (lower-bound x)
		   (upper-bound x))))


(define x (make-interval 0 0))
(define y (make-interval 3 3))

(display (format '#f "~%begin testing of 7-11.scm~%"))

(display-interval x)
(newline)
(display-interval y)
