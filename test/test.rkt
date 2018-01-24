#lang racket/base

(module+ test
  (require rackunit
           "../match.rkt"))

;; match
(module+ test
  (check-equal? (match 1 [(var: x) x]) 1)
  (check-equal? (match (list) [empty 5]) 5)
  (check-equal? (match (list 1) [(list: (var: x)) x]) 1)
  (check-equal? (match (list 1 2 3 4)
                  [(list*: (var: x) (var: y) (var: z))
                   (list x y z)])
                (list 1 2 (list 3 4)))
  (check-equal? (match 1
                  [(?: number? (var: x)) x]
                  [_ "fail"])
                1)
  (check-equal? (match "watermelon"
                  [(?: number? _) "it's a number"]
                  [_ "fail"])
                "fail")
  (check-equal? (match (list 1 2 3)
                  [(or: (list: (var: x) (==: 2) (var: y))
                        (list: (var: x) (var: y) (==: 2)))
                   (list x y)]
                  [_ "fail"])
                (list 1 3))
  (check-equal? (match (list 1 4 2)
                  [(or: (list: (var: x) (==: 2) (var: y))
                        (list: (var: x) (var: y) (==: 2)))
                   (list x y)]
                  [_ "fail"])
                (list 1 4))
  (check-equal? (match (list (list 'a 1) (list 'b 2) (list 'c 3))
                  [(list: (list: (var: k) (var: v)) ...)
                   (list k v)]
                  [_ "fail"])
                (list (list 'a 'b 'c) (list 1 2 3)))
  (check-equal? (match '(1 2 3) [(list: (==: 1) (var: a) ...) a]) '(2 3))
  (check-equal? (match '(1 2 3 4 5)
                  [(list: (==: 1) (var: a) ... (==: 5)) a])
                '(2 3 4))
  (check-equal? (match '(1 (2) (3) (4) 5)
                  [(list: (==: 1) (list: (var: a)) ... (==: 5)) a])
                '(2 3 4))
  (check-equal? (match '(1 2 3 4 . 5)
                  [(list*: (var: a) ... (var: b)) (list a b)])
                '((1 2 3 4) 5))
  (check-equal? (match '(1 2 3 4 5 6 7)
                  [(list: (var: a) ... (==: 5) (var: b) ...)
                   (list a b)])
                '((1 2 3 4) (6 7)))
  (check-equal? (match '(0 2 4 6 8 1 2 3 4)
                  [(list: (?: even? (var: a)) ... (var: b) ...)
                   (list a b)])
                '((0 2 4 6 8) (1 2 3 4)))
  )

;; any/p
(module+ test
  (check-equal? (any/p 3241) '())
  (check-equal? (any/p "watermelon") '()))

;; var/p
(module+ test
  (check-equal? (var/p 3241) '(3241))
  (check-equal? (var/p "watermelon") '("watermelon")))

;; equal/p
(module+ test
  (check-equal? ((equal/p 5) 5) '())
  (check-equal? ((equal/p 5) 71) #false))

;; ?/p
(module+ test
  (check-equal? ((?/p number? var/p) 5) '(5))
  (check-equal? ((?/p number? var/p) "watermelon") #false))

;; and/p
(module+ test
  (check-equal? ((and/p (?/p integer? var/p) (?/p positive? var/p)) 3)
                '(3 3))
  (check-equal? ((and/p (?/p integer? var/p) (?/p positive? var/p)) 3.2)
                #false)
  (check-equal? ((and/p (?/p integer? var/p) (?/p positive? var/p)) -2)
                #false))

;; or/p
(module+ test
  (check-equal? ((or/p (?/p integer? var/p) (?/p positive? var/p)) 3)
                '(3))
  (check-equal? ((or/p (?/p integer? var/p) (?/p positive? var/p)) 3.2)
                '(3.2))
  (check-equal? ((or/p (?/p integer? var/p) (?/p positive? var/p)) -2)
                '(-2))
  (check-equal? ((or/p (?/p integer? var/p) (?/p positive? var/p)) -2.5)
                #false))

;; not/p
(module+ test
  (check-equal? ((not/p (?/p number? var/p)) 5) #false)
  (check-equal? ((not/p (?/p number? var/p)) "watermelon") '()))

;; empty/p
(module+ test
  (check-equal? (empty/p '()) '())
  (check-equal? (empty/p "watermelon") #false))

;; cons/p
(module+ test
  (check-equal? ((cons/p var/p var/p) (cons 1 2)) '(1 2))
  (check-equal? ((cons/p var/p var/p) '()) #false))

;; list/p
(module+ test
  (check-equal? ((list/p var/p var/p var/p) (list 1 2 3)) '(1 2 3))
  (check-equal? ((list/p var/p var/p var/p) (list 1 2)) #false))

;; listof/p
(module+ test
  (check-equal? ((listof/p var/p) (list 1 2 3)) '((1 2 3)))
  (check-equal? ((listof/p (list/p var/p var/p))
                 (list (list 'a 1) (list 'b 2) (list 'c 3)))
                '((a b c) (1 2 3))))

;; listof/rest/p
(module+ test
  (check-equal? ((listof/rest/p var/p empty/p) (list 1 2 3)) '((1 2 3)))
  (check-equal? ((listof/rest/p (list/p var/p var/p) empty/p)
                 (list (list 'a 1) (list 'b 2) (list 'c 3)))
                '((a b c) (1 2 3)))

  (check-equal? ((listof/rest/p var/p (list/p var/p)) (list 1 2 3))
                '((1 2) 3))
  (check-equal? ((listof/rest/p (list/p var/p var/p) (list/p var/p))
                 (list (list 'a 1) (list 'b 2) (list 'c 3)))
                '((a b) (1 2) (c 3))))

