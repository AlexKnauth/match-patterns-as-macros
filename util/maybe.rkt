#lang agile

(provide ?do ?append)

;; do notation for the Maybe monad, pronouned maybe-do
(define-syntax-parser ?do
  #:datum-literals [<-]
  [(_ e:expr)                     #'e]
  [(_ [x:id <- e:expr] rst ...+)  #'(let ([x e]) (and x (?do rst ...)))])

;; ?append : [Maybe [Listof X]] ... -> [Maybe [Listof X]]
(define-simple-macro (?append a:expr ...)
  #:with [av ...] (generate-temporaries #'[a ...])
  (?do [av <- a] ... (append av ...)))

