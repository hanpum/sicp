;;; compute sumer of bigger number in given three number
(define (plus-bigger-2 a b c)
  (cond ((and (< a b) (< a c))
	  (+ b c))
	((and (< b a) (< b c))
	  (+ a c))
	(else (+ a b))))

(plus-bigger-2 1 2 3)
(plus-bigger-2 2 1 3)
(plus-bigger-2 3 2 1)
