#lang agile

(provide (all-defined-out))

(require "util/maybe.rkt" "util/stx.rkt")

(define-for-syntax (get-vars stx)
  (map syntax-local-introduce (syntax-property stx 'vars)))

(define-for-syntax (put-vars stx vars)
  (syntax-property stx 'vars (stx-map syntax-local-introduce vars)))

(define-syntax-parser vars
  [(_ (x ...) body) (put-vars #'body #'[x ...])])

(begin-for-syntax
  (define-pattern-expander ~vars
    [(_ xs-pat body-pat)
     #:with stx (generate-temporary 'vars)
     #'(~and stx (~parse xs-pat (get-vars #'stx)) body-pat)]))

;; --------------------------------------------------------------

;; match
(begin-for-syntax
  (define-syntax-class pat  #:attributes [[x 1] value]
    [pattern (~and pat:expr (~not :ooo))
      #:with (~vars (x:id ...) value:expr) (expand-expr #'pat)]))

(define-simple-macro (match e:expr [pat:pat body:expr] ...)
  (match/fn e (list (list pat.value (λ (pat.x ...) body)) ...)))

(define (match/fn v clauses)
  (cond
    [(empty? clauses) (error 'match "no clause matched the value: ~v" v)]
    [else
     (define pat  (first (first clauses)))
     (define body (second (first clauses)))
     (let ([patv (pat v)])
       (cond [patv (apply body patv)]
             [else (match/fn v (rest clauses))]))]))

;; --------------------------------------------------------------

(define-id-macro _ (vars () any/p))

(define-simple-macro (var: x:id) (vars (x) var/p))

(define-simple-macro (==: val:expr) (vars () (equal/p val)))

(define-simple-macro (?: pass?:expr pat:pat)
  (vars (pat.x ...) (?/p pass? pat.value)))

(define-simple-macro (and: a:pat ...)
  (vars (a.x ... ...) (and/p a.value ...)))

;; TODO: allow the disjunct-patterns to have re-ordered sets of xs
(define-simple-macro (or: a:pat ...+)
  #:when (stx-andmap* free-identifier=? #'[[a.x ...] ...])
  #:with [x ...] (stx-car #'[[a.x ...] ...])
  (vars (x ...) (or/p a.value ...)))

(define-simple-macro (not: a:pat) (vars () (not/p a.value)))

(define-id-macro empty (vars () empty/p))

(define-simple-macro (cons: a:pat b:pat)
  (vars (a.x ... b.x ...) (cons/p a.value b.value)))

(define-syntax-parser list:
  [(_ a:pat ...)
   #'(vars (a.x ... ...) (list/p a.value ...))]
  [(_ a:pat ... (~seq b:pat ooo:ooo c:pat ...) ...)
   #:with [[x ...] ...]   #'[[a.x ... ...] [b.x ... c.x ... ...] ...]
   #:with [[val ...] ...] #'[[a.value ...] [(repeat b.value) c.value ...] ...]
   #'(vars (x ... ...)
       (list/rep/p (list val ... ...)))])

(define-syntax-parser list*:
  [(list*: a:pat ... b:pat)
   #'(vars (a.x ... ... b.x ...) (list*/p (list a.value ...) b.value))]
  [(list*: a:pat ... (~seq b:pat ooo:ooo c:pat ...) ... d:pat)
   #:with [[x ...] ...]   #'[[a.x ... ...] [b.x ... c.x ... ...] ...]
   #:with [[val ...] ...] #'[[a.value ...] [(repeat b.value) c.value ...] ...]
   #'(vars (x ... ... d.x ...)
       (list*/rep/p (list val ... ...) d.value))])

;; --------------------------------------------------------------

;; any/p : Pat
(define (any/p v) '())

;; var/p : Pat
(define (var/p v) (list v))

;; equal/p : Any -> Pat
(define ((equal/p a) v) (and (equal? a v) '()))

;; ?/p : [Any -> Bool] Pat -> Pat
(define ((?/p passes? pat) v) (and (passes? v) (pat v)))

;; and/p : Pat ... -> Pat
(define ((and/p . pats) v)
  (let loop ([pats pats])
    (cond [(empty? pats) '()]
          [else (?append ((first pats) v) (loop (rest pats)))])))

;; or/p : Pat ... -> Pat
(define ((or/p . pats) v)
  (let loop ([pats pats])
    (cond [(empty? pats) #false]
          [else (or ((first pats) v) (loop (rest pats)))])))

;; not/p : Pat -> Pat
(define ((not/p pat) v) (if (pat v) #false '()))

;; empty/p : Pat
(define (empty/p v) (and (empty? v) '()))

;; cons/p : Pat Pat -> Pat
(define ((cons/p a b) v)
  (and (cons? v) (?append (a (car v)) (b (cdr v)))))

;; list/p : Pat ... -> Pat
(define (list/p . pats)
  (foldr cons/p empty/p pats))

;; list*/p : [Listof Pat] Pat -> Pat
(define (list*/p init-pats last-pat)
  (foldr cons/p last-pat init-pats))

;; listof/p : Pat -> Pat
(define ((listof/p pat) v)
  (?do [seq-vars <- (?map pat v)]
       (apply map list seq-vars)))

;; listof/rest/p : Pat Pat -> Pat
(define ((listof/rest/p elem-pat rst-pat) v)
  ;; [Listof X] [Listof [Listof Any]] -> [Maybe [Listof [Listof Any]]]
  (define (process-elements lst acc)
    (cond [(empty? lst) (process-rest lst acc)]
          [(cons? lst)
           (let ([fst (elem-pat (car lst))])
             (if fst
                 (or (process-elements (cdr lst) (cons fst acc))
                     (process-rest lst acc))
                 (process-rest lst acc)))]
          [else
           (process-rest lst acc)]))
  ;; [Listof X] [Listof [Listof Any]] -> [Maybe [Listof [Listof Any]]]
  (define (process-rest lst acc)
    (?append (apply map list (reverse acc)) (rst-pat lst)))
  (process-elements v '()))

(struct repeat [pat] #:transparent)

(define (list/rep/p pats)
  (cond [(empty? pats)  empty/p]
        [(repeat? (first pats))
         (if (empty? (rest pats))
             (listof/p (repeat-pat (first pats)))
             (listof/rest/p (repeat-pat (first pats)) (list/rep/p (rest pats))))]
        [else
         (cons/p (first pats) (list/rep/p (rest pats)))]))

(define (list*/rep/p pats rst-pat)
  (cond [(empty? pats)  rst-pat]
        [(repeat? (first pats))
         (listof/rest/p (repeat-pat (first pats)) (list*/rep/p (rest pats) rst-pat))]
        [else
         (cons/p (first pats) (list*/rep/p (rest pats) rst-pat))]))

;; --------------------------------------------------------------

