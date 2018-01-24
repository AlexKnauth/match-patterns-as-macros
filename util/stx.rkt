#lang agile

(provide define-id-macro
         (for-meta 1 ooo
                     define-pattern-expander
                     expand-expr
                     stx-andmap*
                     (all-from-out syntax/parse/define
                                   syntax/stx
                                   syntax/transformer))
         (for-meta 2 (all-from-out racket/base racket/syntax)))

(require (for-meta 1 syntax/parse/define syntax/stx syntax/transformer)
         (for-meta 2 racket/base racket/syntax))

(define-simple-macro (define-id-macro m:id e:expr)
  (define-syntax m (make-variable-like-transformer #'e)))

(begin-for-syntax
  (define-syntax-class ooo [pattern (~literal ...)])

  (define-simple-macro (define-pattern-expander name:id clause ...)
    (define-syntax name
      (pattern-expander
       (syntax-parser clause ...))))

  (define (expand-expr stx)
    (local-expand stx 'expression '()))

  (define (stx-andmap* f stx-lsts)
    (apply andmap f (stx-map stx->list stx-lsts))))