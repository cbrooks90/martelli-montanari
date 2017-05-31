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
      (let-values ([(eqn e-u? e-s?) (find-eqn (car vars) unsolved solved)])
        (merge (cdr vars)
               (- (+ refs (eqn-count e)) 1)
               (if (memq v reps) reps (cons v reps))
               (append rhs (eqn-rhs e))
               (if e-u? (remove e unsolved) unsolved)
               (if e-s? (remove e solved) solved)
               (append equiv-to-v (cdr (or (assq v equiv-vars) (list #f))))
               (remq (assq v equiv-vars) equiv-vars)
               (if e-u? (merge-vars (vars-in (eqn-rhs e)) vars-to-rem) vars-to-rem)
               (if e-s? (merge-vars (vars-in (eqn-rhs e)) vars-to-add) vars-to-add)))))

(define (factor terms u s)
  (let ([vars (filter var? terms)])
    (if (null? vars)
        (let ([head (common-prefix (prefix (car terms)) (cdr terms))])
          (case head
            [(pair)
             (let-values ([(car-c u s) (factor (map car terms) u s)])
               (let-values ([(cdr-c u s)
                             (if s (factor (map cdr terms) u s)
                                 (values #f '() #f))])
                 (values (cons car-c cdr-c) u s)))]
            [(#f) (values head u #f)]
            [else (values (unprefix head) u s)]))
        (let ([not-vars (filter (lambda (x) (not (var? x))) terms)])
          (merge vars 0 '() not-vars u s '() equiv-vars (vars-in not-vars) '())))))

(define (unify to-do eqns)
  (if (null? to-do) eqns
      (let ([e (find (lambda (x) (= 0 (eqn-count x))) to-do)])
        (and e
             (let-values ([(c to-do eqns) (factor (eqn-rhs e) (remove e to-do) eqns)])
               (and eqns
                    (unify to-do (if (eqn-var e)
                                     (cons (eqn (eqn-var e) 0 (list c)) eqns)
                                     eqns))))))))

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
