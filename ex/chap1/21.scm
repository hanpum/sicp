(load "lib")
(load-option 'format)

(define (report-prime cost)
  (display (format '#f "time cost: ~S" cost)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (timed-prime-test n)
  (display (format '#f "begin to prime test of ~S \n" n))
  (start-prime-test n (runtime)))

(timed-prime-test 1999)
