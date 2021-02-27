(load "utils")

(define (make-mobile left right) (list left right))

(define (make-branch length structure) (list length structure))

;; (a)
(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cdr mobile))
(define (branch-length branch) (car branch))
(define (branch-struct branch) (cdr branch))


;; test whether given struct is a mobile or just a simple weight
(define struct-is-mobile? pair?)


;; (b)
;; compute the weight of given struct
(define (struct-weight struct)
  (if (struct-is-mobile? struct)
      (total-weight struct)
      struct))


;; compute the weight of given branch
(define (branch-weight branch)
  (struct-weight (branch-struct branch)))


;; compute the moment of given branch
(define (branch-moment branch)
  (* (branch-length branch)
     (branch-weight branch)))


;; compute the total weight of given mobile
(define (total-weigth mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))


;; (c)
(define (mobile-balance? mobile)
  (let ((lbranch (left-branch mobile))
	(rbranch (right-branch mobile)))
    (and (equal? (branch-moment lbranch)
		 (branch-moment rbranch))
	 (mobile-balance? lbranch)
	 (mobile-balance? rbranch))))


(println "~%begin testing of 29")
