(load "utils")


;; dot-product of two vector
(define (dot-product v w)
  (accumulate + 0 (map * v w)))


;; matrix multiply a vector
(define (matrix-*-vector m v)
  (map (lambda (row)
	 (dot-product row v))
       m))


;; get matrix transpose 
(define (transpose m)
  (accumulate-n cons '() m))


;; multiply two matrix
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    ;; map on all rows of m
    (map (lambda (mrow)
	   ;; map on all cols of n
	   (map (lambda (ncol)
		  (dot-product mrow ncol))
		cols))
	 m)))


(define (print-matrix matrix)
  (for-each (lambda (row)
	      (println row))
	    matrix))


(println "~%begin testing of 37")


(define mat '((1 2 3 4)
	       (4 5 6 6)
	       (6 7 8 9)))

(define tmat (transpose mat))

(define vec '(1 2 3 4))


(println "~%input matrix mat:")
(print-matrix mat)

(println "~%input vector vec: ~S" vec)

(println "~%dot-prodot of vec vec: ~S" (dot-product vec vec))

(println "~%transpose of mat:")
(print-matrix tmat)

(println "~%mat * vec: ~S" (matrix-*-vector mat vec))

(println "~%mat * mat':")
(print-matrix (matrix-*-matrix mat tmat))
