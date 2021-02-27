(load-option 'format)

;; compute power of given number: base^n, using +,-,*,/ only
(define (^ base n)
  (define (iter base n answer)
    (if (= n 0) answer
	(iter base (- n 1) (* base answer))))
  (iter base n 1))


(define (get-component val base answer)
  (let ((next-val (/ val base)))
    (if (integer? next-val)
	(get-component next-val base (+ answer 1))
	answer)))


(define (cons a b)
  (* (^ 2 a)
     (^ 3 b)))


(define (car n)
  (get-component n 2 0))

(define (cdr n)
  (get-component n 3 0))


;; test function
(define (test a b)
  (let ((n (cons a b)))
    (display (format '#f "~%cons(~S,~S) = ~S, car: ~S, cdr: ~S~%" a b n (car n) (cdr n)))))


(test 0 0)
(test 0 1)
(test 1 0)
(test 1 2)
(test 2 1)
