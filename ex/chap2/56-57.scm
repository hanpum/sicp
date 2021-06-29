(load "utils")

(define (=number? exp val)
  (and (number? exp)
       (= exp val)))


(define (variable? e) (symbol? e))
(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))


(define (sum? e) (eq? '+ (car e)))
(define (addend e) (cadr e))
(define (augend e)
  (let ((rest (cddr e)))
    (if (length=? rest 1)
	(car rest)
	(append '(+) rest))))


(define (make-sum a1 a2)
  (cond
   ((=number? a1 0) a2)
   ((=number? a2 0) a1)
   ((and (number? a1) (number a2)) (+ a1 a2))
   (else (list '+ a1 a2))))


(define (product? e) (eq? '* (car e)))
(define (multiplier e) (cadr e))
(define (multiplicand e)
  (let ((rest (cddr e)))
    (if (length=? rest 1)
	(car rest)
	(append '(*) rest))))

(define (make-product m1 m2) 
  (cond ((=number? m1 1) m2)
	((=number? m2 1) m1)
	((or (=number? m1 0) (=number? m2 0)) 0)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else (list '* m1 m2))))


(define (expt? exp) (eq? '** (car exp)))
(define (base exp) (cadr exp))
(define (exponent exp) (caddr exp))

(define (make-expt base exp)
  (cond ((=number? base 0) 0)
	((=number? base 1) 1)
	((=number? exp 0) 1)
	((=number? exp 1) base)
	((and (number? base) (number? exp)) (expt base exp))
	(else (list '** base exp))))


;; compute the derivant of exp on var
(define (deriv exp var)
  (cond ((number? exp) 0)

	((variable? exp)
	 (if (same-variable? exp var) 1 0))

	((sum? exp)
	 (make-sum (deriv (addend exp) var)
		   (deriv (augend exp) var)))

	((product? exp)
	 (make-sum
	  (make-product (multiplier exp)
			(deriv (multiplicand exp) var))
	  (make-product (multiplicand exp)
			(deriv (multiplier exp) var))))

	((expt? exp)
	 (make-product
	  (make-product (exponent exp)
			(make-expt (base exp)
				   (- (exponent exp) 1)))
	  (deriv (base exp) var)))))


(println "~%begin testing of 56.scm")

(define (do-test exp var)
  (println "deriv ~S on ~S: ~S" exp var (deriv exp var)))


(do-test '(+ x 3) 'x)
(do-test '(* x y) 'x)
(do-test '(* (* x y) (+ x 3)) 'x)
(do-test '(** x 5) 'x)
(do-test '(** (* x y) 5) 'x)

(println (deriv '(* x y (+ x 3)) 'x))
