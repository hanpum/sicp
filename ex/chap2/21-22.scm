(load "utils")

;; ex 2.21/1
(define (square-list2 items)
  (if (null? items)
      '()
      (cons (square (car items))
	    (square-list2 (cdr items)))))


;; ex 2.21/2
(define (square-list1 items)
  (map (lambda (x) (square x))
       items))


;; ex 2.22/1
;; raw solution
(define (square-list3 items)
  (define (iter things answer)
    (if (null? things)
	answer
	(iter (cdr things)
	      (cons (square (car things))
		    answer))))
  (iter items '()))


;; ex 2.22/2, raw solution 2
;; list is sytax sugar of (cons a (cons b (cons c '()))) => (list a b c)
;; below result is: (cons (cons (cons '() 1) 4) 9) => ((('() 1) 4) 9)
(define (square-list4 items)
  (define (iter things answer)
    (if (null? things)
	answer
	(iter (cdr things)
	      (cons answer
		    (square (car things))))))
  (iter items '()))


;; ex 2.22/3 
;; recursive style
(define (square-list5 items)
  (define (iter things)
    (if (null? things)
	'()
	(cons (square (car things))
	      (iter (cdr things)))))
  (iter items))


;; ex 2-22/4
;; iteraction style 
(define (square-list6 items)
  (define (iter things answer)
    (if (null? things)
	answer
	(iter (cdr things)
	      (append! answer
		       (list (square (car things)))))))
  (iter items '()))


(define data (list 1 2 3 4 5))

;; TODO: abstract to a function
;; how to transfer between symbol:'square-list1, object:square-list1, symbol-name:"square-list1"
(println "square-list1 of ~S: ~S" data (square-list1 data))
(println "square-list2 of ~S: ~S" data (square-list2 data))
(println "square-list3 of ~S: ~S" data (square-list3 data))
(println "square-list4 of ~S: ~S" data (square-list4 data))
(println "square-list5 of ~S: ~S" data (square-list5 data))
(println "square-list6 of ~S: ~S" data (square-list6 data))
