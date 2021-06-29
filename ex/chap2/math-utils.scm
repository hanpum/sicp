(load-option 'format)

;; attach type tage to given expression
(define (attach-tag tag exp) (cons tag exp))

;; get type tag from given expression
(define (type-tag exp) (car exp))

;; get contents(data exclude type tag) of given expression
(define (contents exp) (cdr exp))

;; is given expression is a variable
(define (variable? exp) (symbol? exp))

;; is gien two expression are same variable
(define (same-variable? var1 var2)
  (and (variable2 var1)
       (variable? var2)
       (eq? var1 var2)))

;; put value with key1 and key2
(define (put key1 key2 val)
  (2d-put! key1 key2 val))

;; get value associate with key1 and key2
(define (get key1 key2 val)
  (2d-get key1 key2 val))
