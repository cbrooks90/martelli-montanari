(define var vector)
(define var? vector?)
(define (var=? v1 v2) (eqv? (vector-ref v1 0) (vector-ref v2 0)))

(define (eqn v vs c ts) `(,v ,vs ,c . ,ts))
(define eqn-var car)
(define eqn-vars cadr)
(define eqn-count caddr)
(define eqn-rhs cdddr)

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

(define (rem-refs li vars)
  (if (null? li) '()
      (let* ([e (car li)]
             [v (eqn-var e)] [vs (eqn-vars e)] [c (eqn-count e)] [rhs (eqn-rhs e)]
             [pr (assp (lambda (x) (var=? x v)) vars)]
             [δ (if pr (cdr pr) 0)])
        (cons (eqn v vs (max (- c δ) 0) rhs) (rem-refs (cdr li) vars)))))

(define (add-refs li vars)
  (if (null? li) '()
      (let* ([e (car li)]
             [v (eqn-var e)] [vs (eqn-vars e)] [c (eqn-count e)] [rhs (eqn-rhs e)]
             [pr (assp (lambda (x) (var=? x v)) vars)]
             [δ (if pr (cdr pr) 0)]
             [vars (if pr (remp (lambda (x) (var=? (car x) v)) vars) vars)]); Is this still necessary?
        (cons (eqn v vs (+ c δ) rhs) (add-refs (cdr li) vars)))))

(define (v-in-list v li)
  (or (find (lambda (e)
              (or (var=? v (eqn-var e)) (member v (eqn-vars e)))) li)
      (eqn v '() 0 '())))

(define (find-eqn v u s)
  (let ([u-eqn (v-in-list v u)])
    (if u-eqn (values #t #f u-eqn)
        (let ([s-eqn (v-in-list v s)])
          (if s-eqn (values #f #t s-eqn)
              (values #f #f (eqn v '() 0 '())))))))

(define (merge vars u s u-vars s-vars var~ vars~ count~ rhs~)
  (if (null? vars)
      (let ([e (eqn var~ (remove var~ vars~) count~ rhs~)])
        (if (or (null? rhs) (null? (cdr rhs)))
            (values var~ (rem-refs u u-vars) (cons e (rem-refs s u-vars)))
            (values var~ (cons e (add-refs u s-vars)) (add-refs s s-vars))))
      (let-values ([(e e-u? e-s?) (find-eqn (car vars) u s)])
        (merge (cdr vars)
               (if e-u? (remove e u) u)
               (if e-s? (remove e s) s)
               (if e-u? (merge-vars (vars-in (eqn-rhs e)) u-vars) u-vars)
               (if e-s? (merge-vars (vars-in (eqn-rhs e)) s-vars) s-vars)
               (eqn-var e)
               (append (list (eqn-var e)) (eqn-vars e) vars~)
               (- (+ count~ (eqn-count e)) 1)
               (append rhs (eqn-rhs e))))))

(define (factor terms u s)
  (let-values ([(vars not-vars) (filter-vars terms)])
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
        (merge vars u s (vars-in not-vars) '() #f '() 0 not-vars))))

(define (unify to-do eqns)
  (if (null? to-do) eqns
      (let ([e (find (lambda (x) (= 0 (eqn-count x))) to-do)])
        (and e
             (let-values ([(c to-do eqns) (factor (eqn-rhs e) (remove e to-do) eqns)])
               (and eqns
                    (unify to-do (if (eqn-var e)
                                     (cons (eqn (eqn-var e) (eqn-vars e) 0 (list c)) eqns)
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
