(load "utils")

;; the function defined in utils.scm

(println "~%begin testing of 36")

(define data '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))

(println "accumulate-n of ~S: ~S" data (accumulate-n + 0 data))
