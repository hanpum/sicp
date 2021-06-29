(load "utils")

;; definition for input pairs (symbol pair)
(define (symbol-pair pair) (car pair))
(define (weight-pair pair) (cadr pair))
(define (make-pair symbol weight) (cons symbol weight))


;; the definition of leaf ('leaf symbol weight)
(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? node) (eq? 'leaf (car node)))
(define (leaf-symbol node) (cadr node))
(define (leaf-weight node) (caddr node))


;; the definition of tree, e.g, non-tree node, (left right symbols weight)
(define (symbols tree)
  (if (leaf? tree)
      (list (leaf-symbol tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (leaf-weight tree)
      (cadddr tree)))

(define (tree-left-branch tree)
  (if (leaf? tree)
      '()
      (car tree)))

(define (tree-right-branch tree)
  (if (leaf? tree)
      '()
      (cadr tree)))

(define (make-code-tree left right)
  (cond ((null? left) right)
	((null? right) left)
	(else (list left
		    right
		    (append (symbols left)
			    (symbols right))
		    (+ (weight left)
		       (weight right))))))


(define (print-tree tree)
  (print-tree-with-op tree
		      tree-left-branch
		      tree-right-branch
		      (lambda (node)
			(list
			 (symbols node)
			 (weight node)))))


;; decode bits with given code-tree
;;
;; get next branch
;; if next branch is leaf
;;    attach the symbol on it to final result, and contitue decode from root
;; else
;;    continue decode from next branch
(define (decode bits code-tree)
  (define (choose-branch bit tree)
    (cond ((= bit 0) (tree-left-branch tree))
	  ((= bit 1) (tree-right-branch tree))
	  (else (error "bad bit -- CHOOSE-BRANCH" bit))))
  (define (decode-inner bits cur-branch)
    (if (null? bits)
	'()
	(let ((rest-bits (cdr bits))
	      (next-branch (choose-branch (car bits) cur-branch)))
	  (if (leaf? next-branch)
	      (append (symbols next-branch)
		      (decode-inner rest-bits code-tree))
	      (decode-inner rest-bits next-branch)))))
  (decode-inner bits code-tree))


(define (encode message code-tree)
  ;; - current branch is leaf, output the code
  ;; - left branch contain sym, append code with 0 
  ;; - right branch contain sym, append code with 1
  ;; - no branch contain sym, raise an error
  (define (encode-symbol ch branch)
    (if (leaf? branch)
	'()
	(let ((left (tree-left-branch branch))
	      (right (tree-right-branch branch)))
	  (cond ((element-of-list? ch (symbols left))
		 (cons 0 (encode-symbol ch left)))
		((element-of-list? ch (symbols right))
		 (cons 1 (encode-symbol ch right)))
		(else (error "bad message -- ENCODE-SYMBOL" ch))))))
  (if (null? message)
      '()
      (append (encode-symbol (car message) code-tree)
	      (encode (cdr message) code-tree))))


;; functions used to build code tree
(define (add-to-order-tree-set x tree-set)
  (if (null? tree-set)
      (list x)
      (if (<= (weight x) (weight (car tree-set)))
	  (cons x tree-set)
	  (cons (car tree-set)
		(add-to-order-tree-set x (cdr tree-set))))))


;; build leaf set from input pairs 
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (add-to-order-tree-set
       (make-leaf (symbol-pair (car pairs))
		  (weight-pair (car pairs)))
       (make-leaf-set (cdr pairs)))))


;; merge a set of tree to build huffman tree
(define (successive-merge ordered-tree-set)
  (let ((len (length ordered-tree-set)))
    (cond ((= len 0) '())
	  ((= len 1) (car ordered-tree-set))
	  (else (successive-merge
		 (add-to-order-tree-set
		  (make-code-tree (cadr ordered-tree-set)
				  (car ordered-tree-set))
		  (cddr ordered-tree-set)))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))


(println "~%begin testing of 67")

;; 2.67
(define sample-tree (make-code-tree (make-leaf 'A 4)
				    (make-code-tree (make-leaf 'B 2)
						    (make-code-tree (make-leaf 'D 1)
								    (make-leaf 'C 1)))))

(define sample-pairs '((A 4) (C 1) (D 1) (B 2)))
(define sample-bits '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(println "code tree:")
(print-tree sample-tree)

(define sample-msgs (decode sample-bits sample-tree))
(println "decode message ~S result: ~S" sample-bits sample-msgs)


;; 2.68
(println "~%begin testing of 68")
(define encode-bits (encode sample-msgs sample-tree))
(println "encode ~S result: ~S ~A" sample-msgs encode-bits
	 (if (equal? sample-bits encode-bits)
	     "PASSED"
	     "FAILED"))

;; should failed on F
;; (encode '(A B C D E F) sample-tree)


;; 2.69
(println "~%begin testing of 69")
(println "generate huffman tree from ~S" sample-pairs)
(println "result:")
(print-tree (generate-huffman-tree sample-pairs))


;; 2.70
(println "~%begin testing of 70")
(define sample-pairs '((a 2) (na 16) (boom 1) (sha 3) (get 2) (yip 9) (job 2) (wah 1)))
(define code-tree (generate-huffman-tree sample-pairs))
(define message (append
		 '(get a job)
		 '(sha na na na na na na na na)
		 '(get a job)
		 '(sha na na na na na na na na)
		 '(wah yip yip yip yip yip yip yip yip yip)
		 '(sha boom)))

(print-tree code-tree)

(map (lambda (item)
       (let ((sym (car item)))
	 (println "encode of ~S: ~S"
		  item
		  (encode (list sym) code-tree))))
     sample-pairs)

(println "encode of ~S, result: ~S"
	 message
	 (encode message code-tree))
