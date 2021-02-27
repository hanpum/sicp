(load "utils")

;; function defined in utils.scm


(println "~%begin testing of 38")

(define data '(1 2 3))


(define (test op init opname)
  (println "(fold-left ~S ~S ~S): ~S" opname init data (fold-left op init data))
  (println "(fold-right ~S ~S ~S): ~S" opname init data (fold-right op init data))
  (newline))


(test / 1 "/")
(test list '() "list")

;; op should get same result while evalue from left to right and right to left
(test + 0 "+")
