(load-option 'format)
(load "7-11")

(define (make-center-width c w)
  (make-interval
   (- c w)
   (+ c w)))

(define (center x)
  (/ (+ (lower-bound x) (upper-bound x)) 2.0))

(define (width x)
  (/ (- (upper-bound x) (lower-bound x)) 2.0))


(define (make-center-percent c p)
  (make-center-width c (* c p)))


(define (percent x)
  (/ (width x) (center x)))


(display (format '#f "~%begin testing of 12.scm~%"))

(display-interval (make-center-percent 1 0.01))
