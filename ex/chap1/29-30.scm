(load-option 'format)

(define (do-sum a b stop? next eval accumulate)
  (if (stop? a b)
      accumulate
      (do-sum (next a) b stop? next eval (+ accumulate (eval a)))))


(define (inc a)
  (+ a 1))


;; sum from a to b
(define (int-sum a b)
  (define (identity a) a)
  (do-sum a b > inc identity 0))


;; cube sum from a to b  
(define (cube-sum a b)
  (do-sum a b > inc cube 0))


;; compute pi   pi/8  = 1/(1*3) + 1/(5*7) + 1/(9*11) + ...
(define (compute-pi a b)
  (define (next a) (+ a 4))
  (define (f a)
    (/ 1.0 (* a (+ a 2))))
  (* 8 (do-sum a b > next f 0.0)))


;; integeral function on range [a b]
(define (integeral f a b dx)
  (define (next a) (+ a dx))
  (define (get-val a)
    (f (+ a (/ dx 2.0))))
  (* dx
     (do-sum a b > next get-val 0.0)))


;; do integeration with simpson algoirthm 
(define (simpson-integral f a b n)
  (let ((h (/ (- b a) n)))
    (define (stop? x y)
      (> (cdr x) n))
    (define (next x)
      (cons (+ (car x) h)
	    (+ 1 (cdr x))))
    (define (get-val x)
      (* (cond
	  ((= 0 (cdr x)) 1)
	  ((= n (cdr x)) 1)
	  ((odd? (cdr x)) 4)
	  (else 2))
	 (f (car x))))
    ;; do real work
    (* (/ h 3.0)
       (do-sum (cons a 0) b stop? next get-val 0.0))))


;; (compute-pi 1 1000)
(integeral cube 0 1.0 0.01)
(simpson-integral cube 0.0 1.0 100)
