;; Jason Hemann and Dan Friedman

(load "unification.scm")
(load "old-unification.scm")

(define (initialize vs eqns)
  (if (null? vs) eqns
      (initialize
        (cdr vs)
        (let loop ([es eqns] [v (eqn-var (or (find-class (caar vs) eqns) (car vs)))])
          (if (null? es)
              (list (eqn v '() (cdar vs) '()))
              (let ([e (car es)])
                (if (var=? v (eqn-var e))
                    (cons (eqn v (eqn-vars e) (+ (eqn-count e) (cdar vs)) (eqn-terms e)) (cdr es))
                    (cons e (loop (cdr es) v)))))))))

(define (== t . ts)
  (lambda (s/c)
    (let ([s (unify (list (eqn #f '() 0 (cons t ts)))
                    (initialize (fold-right
                                  (lambda (x acc)
                                    (merge-vars (vars-in x) acc))
                                  '() (cons t ts)) (car s/c)))]
          [s~ (old-unify t (car ts) (cadr s/c))])
      (let ([new-s/c `(,s ,s~ . ,(cddr s/c))])
        (cond [(and s s~)
               (if (equal? (reify-1st new-s/c) (old-reify-1st new-s/c))
                   (unit new-s/c)
                   (errorf #f "Unifications differ\n~a\n~a\n" (car s/c) (cadr s/c)))]
              [(and (not s) (not s~)) mzero]
              [else (errorf #f "Unifications differ\n~a\n~a\n" (car s/c) (cadr s/c))])))))

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
