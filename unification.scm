(define var vector)
(define var? vector?)
(define (var=? v1 v2) (eqv? (vector-ref v1 0) (vector-ref v2 0)))

(define (eqn v c ts) `(,v ,c . ,ts))
(define eqn-var car)
(define eqn-count cadr)
(define eqn-rhs cddr)

(define (prefix term)
  (cond [(pair? term) 'pair]
        [(symbol? term) `(symbol ,term)]
        [(not term) 'false]
        [else term]))

(define (unprefix p)
  (cond [(pair? p) (cadr p)]
        [(eq? p 'false) #f]
        [else p]))

(define (common-prefix first rest)
  (if (null? rest) first
      (let ([p (prefix (car rest))])
        (if (equal? first p)
            (common-prefix p (cdr rest))
            #f))))

(define (rep v equiv-vars)
  (cond [(null? equiv-vars) v]
        [(memp (lambda (x) (var=? x v)) (car equiv-vars)) (caar equiv-vars)]
        [else (rep v (cdr equiv-vars))]))

(define (rem-refs li vars)
  (if (null? li) '()
      (let* ([e (car li)]
             [v (eqn-var e)] [c (eqn-count e)] [rhs (eqn-rhs e)]
             [pr (assp (lambda (x) (var=? x v)) vars)]
             [δ (if pr (cdr pr) 0)])
        (if (> δ c) (error #f "Negative") void)
        (cons (eqn v (max (- c δ) 0) rhs) (rem-refs (cdr li) vars)))))

(define (add-refs li vars)
  (if (null? li) '()
      (let* ([e (car li)]
             [v (eqn-var e)] [c (eqn-count e)] [rhs (eqn-rhs e)]
             [pr (assp (lambda (x) (var=? x v)) vars)]
             [δ (if pr (cdr pr) 0)]
             [vars (if pr (remp (lambda (x) (var=? (car x) v)) vars) vars)])
        (cons (eqn v (+ c δ) rhs) (add-refs (cdr li) vars)))))

(define (merge vars refs reps rhs unsolved solved equiv-to-v equiv-vars vars-to-rem vars-to-add)
  (if (null? vars)
      (let ([e (eqn (car reps) refs rhs)]
            [equiv-vars
             (if (and (null? equiv-to-v) (null? (cdr reps)))
                 equiv-vars
                 (cons (append reps equiv-to-v) equiv-vars))])
        (if (null? rhs)
            (values (car reps) unsolved (cons e solved) equiv-vars)
            (if (null? (cdr rhs))
                (values (car reps)
                        (rem-refs unsolved vars-to-rem)
                        (cons e (rem-refs solved vars-to-rem))
                        equiv-vars)
                (values (car reps)
                        (cons e (add-refs unsolved vars-to-add))
                        (add-refs solved vars-to-add)
                        equiv-vars))))
      (let* ([v (rep (car vars) equiv-vars)]
             [e-from-u (assp (lambda (x) (var=? x v)) unsolved)]
             [e-from-s (and (not e-from-u) (assp (lambda (x) (var=? x v)) solved))]
             [e (or e-from-u e-from-s (eqn v 0 '()))])
        (if e
            (merge (cdr vars)
                   (- (+ refs (eqn-count e)) 1)
                   (if (memq v reps) reps (cons v reps))
                   (append rhs (eqn-rhs e))
                   (if e-from-u (remove e unsolved) unsolved)
                   (if e-from-s (remove e solved) solved)
                   (append equiv-to-v (cdr (or (assq v equiv-vars) (list #f))))
                   (remq (assq v equiv-vars) equiv-vars)
                   (if e-from-u (merge-vars (vars-in (eqn-rhs e)) vars-to-rem) vars-to-rem)
                   (if e-from-s (merge-vars (vars-in (eqn-rhs e)) vars-to-add) vars-to-add))
            void))));(merge (cdr vars) (- refs 1) reps rhs unsolved solved equiv-to-v equiv-vars)))))

(define (factor terms u s equiv-vars top)
  (let ([vars (filter var? terms)])
    (if (null? vars)
        (let ([head (common-prefix (prefix (car terms)) (cdr terms))])
          (case head
            [(pair)
             (let-values ([(car-c u s equiv-vars) (factor (map car terms) u s equiv-vars top)])
               (let-values ([(cdr-c u s equiv-vars)
                             (if s (factor (map cdr terms) u s equiv-vars top)
                                 (values #f u s equiv-vars))])
               (values (cons car-c cdr-c) u s equiv-vars)))]
            [(#f) (values head u #f equiv-vars)]
            [else (values (unprefix head) u s equiv-vars)]))
        (let ([not-vars (filter (lambda (x) (not (var? x))) terms)])
          (merge vars top '() not-vars u s '() equiv-vars (vars-in not-vars) '())))))

(define (solve unsolved solved equiv-vars)
  (if (null? unsolved) (values solved equiv-vars)
      (let ([e (find (lambda (x) (= 0 (eqn-count x))) unsolved)])
        (if e
            (let-values ([(c unsolved~ solved~ equiv-vars~)
                          (factor (eqn-rhs e) (remove e unsolved) solved equiv-vars 0)])
              (if solved~
                  (solve unsolved~ (if (eqn-var e) (cons (eqn (eqn-var e) 0 (list c)) solved~) solved~) equiv-vars~)
                  (values #f '())))
            (values #f '())))))

(define (merge-vars l1 l2)
  (if (null? l1) l2
      (let loop ([v (car l1)] [l2 l2] [accum '()])
        (cond [(null? l2) (merge-vars (cdr l1) (cons v accum))]
              [(var=? (car v) (caar l2))
               (merge-vars
                 (cdr l1)
                 (append
                   accum
                   (cons (cons (car v) (+ (cdr v) (cdar l2)))
                         (cdr l2))))]
              [else (loop v (cdr l2) (cons (car l2) accum))]))))

(define (vars-in t)
  (cond [(null? t) '()]
        [(var? t) `((,t . ,1))]
        [(pair? t) (merge-vars (vars-in (car t)) (vars-in (cdr t)))]
        [else '()]))

(define (initialize vs eqns equiv-vars)
  (if (null? vs) eqns
      (let loop ([es eqns] [v (rep (caar vs) equiv-vars)] [accum '()])
        (if (null? es)
            (initialize (cdr vs) (cons (eqn v (cdar vs) '()) eqns) equiv-vars)
            (let ([e (car es)])
              (if (var=? v (eqn-var e))
                  (initialize
                    (cdr vs)
                    (append
                      accum
                      (cons (eqn v (+ (eqn-count e) (cdar vs)) (eqn-rhs e))
                            (cdr es)))
                    equiv-vars)
                  (loop (cdr es) v (cons e accum))))))))

(define (unify t1 t2 solved equiv-vars)
  (solve (list (eqn #f 0 `(,t1 ,t2)))
         (initialize (merge-vars (vars-in t1) (vars-in t2)) solved equiv-vars)
         equiv-vars))
