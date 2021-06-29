(load-option 'format)


(define (prime? n)
  ;; is a dividable by b?
  (define (divides? a b)
    (= 0 (remainder a b)))

  ;; guess iterally 
  (define (guess-iter n guess)
    (cond ((> guess (square n)) n)
	  ((divides? n guess) guess)
	  (else (guess-iter n (+ 1 guess)))))

  ;; find smallest divisor for given number
  (define (smallest-divisor n)
    (guess-iter n 2))

  ;; if a number's smaller divisor it's itself, then it's prime
  (= n (smallest-divisor n)))


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


;; accumulate result of operation on given sequence
;; OP: a function of (op x rest)
;;     - x: current element in sequence
;;     - rest: the operation result on result of sequence elements
(define (accumulate op initial seq)
  (if (null? seq) initial
      (op (car seq)
	  (accumulate op initial (cdr seq)))))


;; same as accumulate, but execute on multiple sequence
;; - SEQS is a list of sequence
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map (lambda (seq)
				       (car seq))
				     seqs))
	    (accumulate-n op init (map (lambda (seq)
					 (cdr seq))
				       seqs)))))


(define fold-right accumulate)


;; OP: a function of (op result x)
(define (fold-left op initial seq)
  (define (iter op result rest)
    (if (null? rest) result
	(iter op (op result
		     (car rest))
	      (cdr rest))))
  (iter op initial seq))


;; just like stream.map in java
(define (flate-map op seq)
  (accumulate append '() (map op seq)))


;; generate a list of integers [start, start+1, ... n]
(define (enumerate-integers start n)
  (define (iter i buf)
    (if (> i n)
	buf
	(iter (+ i 1)
	      (append buf (list i)))))
  (iter start '()))


;; print items in given list, one per line
(define (println-list seq)
  (map (lambda (item)
	 (println "~S" item))
       seq)
  ;; skip return value of above map call
  '#t)


(define (make-tree entry left right) (list entry left right))

(define (tree-entry tree) (car tree))
(define (tree-left-branch tree) (cadr tree))
(define (tree-right-branch tree) (caddr tree))


;; print a tree with given operator
;; ARGUMENTS:
;; - obj target tree to print
;; - left-op operator to get left branch
;; - right-op operator to get right branch
;; - root-op operator used to build display form of tree
(define (print-tree-with-op obj left-op right-op root-op)
  (define (next-indent d) (+ d 4))
  (define (iter tree indents)
    (if (not (null? tree))
	(begin (iter (left-op tree) (next-indent indents))
	       (println "~VA~S" indents "" (root-op tree))
	       (iter (right-op tree) (next-indent indents)))))
  (iter obj 0))


(define (print-tree obj) (print-tree-with-op obj tree-left-branch tree-right-branch tree-entry))


(define (sorted-list->tree list)
  ;; build a partial tree from sorted elements
  (define (partial-tree elts n)
    (if (= n 0)
	(cons '() elts)
	(let ((left-size (quotient (- n 1) 2)))
	  (let* ((left-result (partial-tree elts left-size))
		 (left-tree (car left-result))
		 (non-left-elts (cdr left-result))
		 (right-size (- n left-size 1)))
	    (let* ((root (car non-left-elts))
		  (right-result (partial-tree (cdr non-left-elts) right-size))
		  (right-tree (car right-result))
		  (remaining-elts (cdr right-result)))
	      (cons (make-tree root left-tree right-tree)
		    remaining-elts))))))
  (car (partial-tree list (length list))))


(define (element-of-list? x list)
  (cond ((null? list) false)
	((equal? x (car list)) true)
	(else (element-of-list? x (cdr list)))))
