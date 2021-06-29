(load "utils")


(define empty-board '())


;; get postion of k'th queene from given arrangements, k start from 1
(define (get-pos positions k)
  (list-ref positions (- k 1)))


;; is two position (row1, col1) and (row2, col2) are conflict
(define (conflict? col1 row1 col2 row2)
  (or (= col1 col2)
      (= row1 row2)
      (= (+ col1 row1) (+ col2 row2))
      (= (- col1 row1) (- col2 row2))))


(define (safe? k positions)
  (define (iter n krow)
    (cond ((= 0 n) '$t)
	  ((conflict? k krow n (get-pos positions n)) '#f)
	  (else (iter (- n 1) krow))))
  (iter (- k 1) (get-pos positions k)))


(define (adjoin-position new-row k rest-of-queenes)
  (append rest-of-queenes (list new-row)))


;; get all posible arrangement of queenes on board of given size
(define (queene board-size)
  ;; get all validate arrangement of first k queenes
  (define (queene-cols k)
    (if (= k 0)
	(list empty-board)
	(filter (lambda (positions) (safe? k positions))
		(flate-map (lambda (rest-of-queenes)
			     (map (lambda (new-row)
				    (adjoin-position new-row k rest-of-queenes))
				  (enumerate-integers 1 board-size)))
			   (queene-cols (- k 1))))))
  (queene-cols board-size))


(define (make-row pos-set size)
  (define (iter n buf)
    (cond ((> n size) buf)
	  ((= n pos-set) (iter (+ n 1)
			       (append buf (list 1))))
	  (else (iter (+ n 1)
		      (append buf (list 0))))))
  (iter 1 '()))


(define (make-board position)
  (map (lambda (pos-set)
	 (make-row pos-set (length position)))
       position))


(define (print-board position)
  (println "~%position: ~S" position)
  (println-list (make-board position)))


(println "~%begin testing of 42.scm")

(map print-board (queene 6))
