;; let-syntax bindings  expression
;; BINDINGS: ((KEYWORD TRANSFORMER-SPEC) ...)
;;
;; syntax-rules literals syntax-rule
;; literals: ()
;; syntax-rule: (PATTERN TEMPLATE)
(let-syntax ((when (syntax-rules ()                     ;; literals is empty
		     ((when test stmt1 stmt2 ...)       ;; PATTERN
		      (if test                          ;; TEMPLATE, replace when with this 
			  (begin (display "begin substitude...")
				 stmt1
				 stmt2 ...))))))
  (let ((if #t))
    (when if (set! if 'now))
    if))


;; (let ((x 'outer))
;;   (let-syntax ((m (syntax-rules () ((m) x))))
;;     (let ((x 'inner))
;;       (m))))

