(load-option 'format)


(define (product a b stop? next eval accumulate)
  (if (stop? a b)
      accumulate
      (product (next a) b stop? next eval
	       (* (eval a) accumulate))))


(define (stop? a b)
  (> (cdr a) b))


(define (next x)
  (cons (+ 1 (cdr x)) (+ 1 (car x))))


(define (eval a)
  (/ (car a) (cdr a)))


(define (pi n)
  (* 4.0
     (product (cons 2 3) n stop? next eval 1.0)))


(pi 1000)
