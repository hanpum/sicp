(load-option 'format)

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

(define data (cons 1 2))

(define data (cons 1 2))

(display (format '#f "~%~S: ~S,~S~%" data (car data) (cdr data)))
