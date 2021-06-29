(load "utils")


(define (record-key record) (car record))
(define (record-data record) (cdr record))
(define (make-record k v) (cons k v))


(define (lookup given-key set-of-records)
  (if (null? set-of-records)
      '()
      (let* ((root (tree-entry set-of-records))
	    (left (tree-left-branch set-of-records))
	    (right (tree-right-branch set-of-records))
	    (root-key (record-key root)))
	(cond ((= given-key root-key) (record-data root))
	      ((< given-key root-key) (lookup given-key left))
	      (else (lookup given-key right))))))


(println "~%begin testing of 66")


(define (create-record-set size)
  (define (iter n)
    (if (> n size)
	'()
	(cons (make-record n n)
	      (iter (+ n 1)))))
  (iter 0))

(define records (create-record-set 10))
(define sets (sorted-list->tree records))

(println "target tree:")
(print-tree sets)
(println "")
(println "lookup 9 result: ~A" (lookup 9 sets))
(println "lookup 11 result: ~A" (lookup 11 sets))
