#lang agile

(provide ?do ?append ?map)

;; do notation for the Maybe monad, pronouned maybe-do
(define-syntax-parser ?do
  #:datum-literals [<-]
  [(_ e:expr)                     #'e]
  [(_ [x:id <- e:expr] rst ...+)  #'(let ([x e]) (and x (?do rst ...)))])

;; ?append : [Maybe [Listof X]] ... -> [Maybe [Listof X]]
(define-simple-macro (?append a:expr ...)
  #:with [av ...] (generate-temporaries #'[a ...])
  (?do [av <- a] ... (append av ...)))

;; ?map : [X -> [Maybe Y]] [Listof X] -> [Maybe [Listof Y]]
(define (?map f lst)
  (cond [(empty? lst) '()]
        [else (?do [f-fst <- (f (first lst))]
                   [f-rst <- (?map f (rest lst))]
                   (cons f-fst f-rst))]))

