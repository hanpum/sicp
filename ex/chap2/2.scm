(load-option 'format)


(define (make-point x y) (cons x y))
(define (x-point pt) (car pt))
(define (y-point pt) (cdr pt))

(define (display-point pt)
  (display (format '#f "(~S,~S)"
		   (x-point pt)
		   (y-point pt))))


(define (make-segment start end) (cons start end))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))

(define (midpoint-segment seg)
  (make-point
   (/ (+ (x-point (start-segment seg))
	 (x-point (end-segment seg)))
      2.0)
   (/ (+ (y-point (start-segment seg))
	 (y-point (end-segment seg)))
      2.0)))


(define (display-segment seg)
  (newline)
  (display-point (start-segment seg))
  (display " --- ")
  (display-point (end-segment seg))
  (newline))


(define seg (make-segment
	     (make-point 0 0)
	     (make-point 1 1)))


(display-segment seg)
(display-point (midpoint-segment seg))
