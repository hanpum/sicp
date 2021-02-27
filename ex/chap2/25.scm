(load "utils")

(define a (list 1 2 (list 5 7 9)))
(println "input: ~S, output: ~S" a (car (cdaddr a)))

(define b (list (list 7)))
(println "input: ~S, output: ~S" b (caar b))

(define c (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(println "input: ~S, output: ~S" c (cadadr (cadadr (cadadr c))))
