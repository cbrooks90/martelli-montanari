;; Jason Hemann and Dan Friedman

(load "unification-mm.scm")

(define (== u v)
  (lambda (s/c)
    (let-values ([(solved equiv) (unify u v (car s/c) (cadr s/c))])
      (if solved (unit `(,solved ,equiv . ,(cddr s/c))) mzero))))

(define (unit s/c) (cons s/c mzero))
(define mzero '())

(define (call/fresh f)
  (lambda (s/c)
    (let ([c (cddr s/c)])
      ((f (var c)) `(,(car s/c) ,(cadr s/c) . ,(+ c 1))))))

(define (disj g1 g2) (lambda (s/c) (mplus (g1 s/c) (g2 s/c))))
(define (conj g1 g2) (lambda (s/c) (bind (g1 s/c) g2)))

(define (mplus $1 $2)
  (cond
    [(null? $1) $2]
    [(procedure? $1) (lambda () (mplus $2 ($1)))]
    [else (cons (car $1) (mplus (cdr $1) $2))]))

(define (bind $ g)
  (cond
    [(null? $) mzero]
    [(procedure? $) (lambda () (bind ($) g))]
    [else (mplus (g (car $)) (bind (cdr $) g))]))
