;; Jason Hemann and Dan Friedman

(load "unification.scm")

(define (initialize vs eqns)
  (if (null? vs) eqns
      (let loop ([es eqns] [v (rep (caar vs) equiv-vars)] [accum '()])
        (if (null? es)
            (initialize (cdr vs) (cons (eqn v (cdar vs) '()) eqns))
            (let ([e (car es)])
              (if (var=? v (eqn-var e))
                  (initialize
                    (cdr vs)
                    (append
                      accum
                      (cons (eqn v (+ (eqn-count e) (cdar vs)) (eqn-rhs e))
                            (cdr es))))
                  (loop (cdr es) v (cons e accum))))))))

(define (== u v)
  (lambda (s/c)
    (let ([s (unify (list (eqn #f '() 0 (list t1 t2)))
                    (initialize (merge-vars (vars-in t1) (vars-in t2)) (car s/c)))])
      (if s (unit `(,s . ,(cdr s/c))) mzero))))

(define (unit s/c) (cons s/c mzero))
(define mzero '())

(define (call/fresh f)
  (lambda (s/c)
    (let ([c (cdr s/c)])
      ((f (var c)) `(,(car s/c) . ,(+ c 1))))))

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
