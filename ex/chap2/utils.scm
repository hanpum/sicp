(load-option 'format)


;; print object to stdout
;;
;; USAGE:
;;   (print fmt obj ...)
(define-syntax print
  (syntax-rules ()
    ((print var) (if (string? var)
		     (display (format '#f var))
		     (display var)))
    ((print fmt var rest ...) (display (format '#f fmt var rest ...)))))


;; same as print, but will append a newline at end
(define-syntax println
  (syntax-rules ()
    ((println var) (begin (if (string? var)
			      (display (format '#f var))
			      (display var))
			  (newline)))
    ((println fmt var rest ...) (begin (display (format '#f fmt var rest ...))
				       (newline)))))


(define fold-right accumulate)


;; OP: a function of (op result x)
(define (fold-left op initial seq)
  (define (iter op result rest)
    (if (null? rest) result
	(iter op (op result
		     (car rest))
	      (cdr rest))))
  (iter op initial seq))



;; accumulate result of operation on given sequence
;; OP: a function of (op x rest)
;;     - x: current element in sequence
;;     - rest: the operation result on result of sequence elements
(define (accumulate op initial seq)
  (if (null? seq) initial
      (op (car seq)
	  (accumulate op initial (cdr seq)))))


(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map (lambda (seq)
				       (car seq))
				     seqs))
	    (accumulate-n op init (map (lambda (seq)
					 (cdr seq))
				       seqs)))))
