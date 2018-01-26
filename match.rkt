#lang agile

(provide (all-defined-out))

(require anaphoric "util/maybe.rkt" "util/stx.rkt")

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
     (cond-let [[patv (pat v)] (apply body patv)]
               [else           (match/fn v (rest clauses))])]))

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

(begin-for-syntax
  (define-splicing-syntax-class pat/repeat  #:attributes [[x 1] value]
    [pattern (~seq p:pat :ooo)
      #:with [[x ...] value] #'[[p.x ...] (repeat p.value)]]
    [pattern (~seq :pat)]))

(define-simple-macro (list: a:pat/repeat ...)
  (vars (a.x ... ...) (list/p a.value ...)))

(define-simple-macro (list*: a:pat/repeat ... b:pat)
  (vars (a.x ... ... b.x ...) (list*/p (list a.value ...) b.value)))

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

(struct repeat [pat] #:transparent)

;; cons/rep/p : (U Pat (repeat Pat)) Pat -> Pat
(define (cons/rep/p a b)
  (cond [(repeat? a) (listof/rest/p (repeat-pat a) b)]
        [else        (cons/p a b)]))

;; list/p : Pat ... -> Pat
(define (list/p . pats)
  (foldr cons/rep/p empty/p pats))

;; list*/p : [Listof Pat] Pat -> Pat
(define (list*/p init-pats last-pat)
  (foldr cons/rep/p last-pat init-pats))

;; listof/rest/p : Pat Pat -> Pat
(define ((listof/rest/p elem-pat rst-pat) v)
  ;; [Listof X] [Listof [Listof Any]] -> [Maybe [Listof [Listof Any]]]
  (define (process-elements lst acc)
    (cond [(empty? lst) (process-rest lst acc)]
          [(cons? lst)
           (if-let [fst (elem-pat (car lst))]
                   (or (process-elements (cdr lst) (cons fst acc))
                       (process-rest lst acc))
                   (process-rest lst acc))]
          [else
           (process-rest lst acc)]))
  ;; [Listof X] [Listof [Listof Any]] -> [Maybe [Listof [Listof Any]]]
  (define (process-rest lst acc)
    (?append (apply map list (reverse acc)) (rst-pat lst)))
  (process-elements v '()))

;; --------------------------------------------------------------

