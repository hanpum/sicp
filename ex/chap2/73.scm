;;

;; ┌──────────────────────────────────────────────────────┐
;; │                                                      │
;; │                ┌────────────────────┐                │
;; ├────────────────┤    APPLICATIONS    ├────────────────┤
;; │                └────────────────────┘                │
;; │                                                      │
;; │                ┌────────────────────┐                │
;; ├────────────────┤    interface api   ├────────────────┤
;; │                └─┬────────────────┬─┘                │
;; │  implemation 1   │      ...       │  implemation n   │
;; │                  │                │                  │
;; └──────────────────┴────────────────┴──────────────────┘
;;
;; interface:
;; - opreator
;; - operands
;; - deriv
;;
;; implementations:
;; - sum: (+ a b c)
;; - product: (* a b c)
;; - exp: (** a b)

(load "math-utils")


(define (operator exp) (car exp))
(define (operands exp) (cdr exp))


(define (install-sum)
  (define (make-sum a1 a2)
    (cond ((number=? a1 0) a2)
	  ((number=? a2 0) a1)
	  ((and (number? a1) (number a2)) (+ a1 a2))
	  (else (list '+ a1 a2))))

  (define (addend e) (cadr e))

  (define (augend e)
    (let ((rest (cddr e)))
      (if (length=? rest 1)
	  (car rest)
	  (append '(+) rest))))

  (define (internal_deriv exp var)
    (make-sum (deriv (addend exp) var)
	      (deriv (augend exp) var)))

  (put 'deriv '+ internal_deriv)
  (put 'make-sum 'any make-sum))



(define (install-product)
  (define (multiplier expr) (cadr expr))

  (define (multiplicand expr)
    (let ((rest (cddr expr)))
      (if (length=? rest 1)
	  (car rest)
	  (append '(*) rest))))

  (define (make-product m1 m2) 
    (cond ((=number? m1 1) m2)
	  ((=number? m2 1) m1)
	  ((or (=number? m1 0) (=number? m2 0)) 0)
	  ((and (number? m1) (number? m2)) (* m1 m2))
	  (else (list '* m1 m2))))

  ;; (f1*f2)' = f1'*f2 + f1*f2'
  (define (internal_deriv exp var)
    (let ((1st (multiplier exp))
	  (2nd (multiplicand exp)))
      (make-sum
       (make-product (deriv 1st var) 2nd)
       (make-product 1st (deriv 2nd var)))))

  (put 'deriv '* internal_deriv)
  (put 'make-product 'any make-product))



(define (install-exp)
  ;;(define (expt? exp) (eq? '** (car exp)))

  (define (base expr) (cadr expr))
  (define (exponent expr) (caddr expr))

  (define (make-expt base expr)
    (cond ((=number? base 0) 0)
	  ((=number? base 1) 1)
	  ((=number? expr 0) 1)
	  ((=number? expr 1) base)
	  ((and (number? base) (number? expr)) (expt base expr))
	  (else (list '** base expr))))

  ;; (f1^n)' =  n * f1^(n-1)  * f1'
  (define (internal_deriv expr var)
    (let ((1st (base expr))
	  (2nd (exponent expr)))
      (make-product 2nd
		    (make-product (make-expt 1st (- 2nd 1))
				  (deriv 1st var)))))

  (put 'deriv '** internal_deriv)
  (put 'make-expt 'any make-expt))


;; application 
(install-sum)
(install-exp)
(install-product)

(define (deriv exp var)
  (cond ((number? exp) 0)

	((variable? exp)
	 (if (same-variable? exp var) 1 0))

	(else ((get 'deriv (operator exp)) (operands exp)
	       var))))



(println "~%begin testing of 73.scm")

(define (do-test exp var)
  (println "deriv ~S on ~S: ~S" exp var (deriv exp var)))


(do-test '(+ x 3) 'x)
(do-test '(* x y) 'x)
(do-test '(* (* x y) (+ x 3)) 'x)
(do-test '(** x 5) 'x)
(do-test '(** (* x y) 5) 'x)

(println (deriv '(* x y (+ x 3)) 'x))
