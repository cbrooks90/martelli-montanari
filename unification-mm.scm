(define var vector)
(define var? vector?)
(define (var-num x) (vector-ref x 0))
(define (var=? v1 v2) (= (var-num v1) (var-num v2)))

(define (eqn v c ts) `(,v ,c . ,ts))
(define eqn-count cadr)
(define eqn-var car)
(define eqn-rhs cddr)

(define (prefix term)
  (cond [(null? term) '()]
        [(pair? term) #t]; Something to distinguish this case from a symbol prefix
        [(integer? term) term]
        [(symbol? term) term]
        [else (error 'prefix "Unrecognized term")]))

(define (common-prefix first rest)
  (if (null? rest)
      first
      (let ([p (prefix (car rest))])
        (if (eqv? first p)
            (common-prefix p (cdr rest))
            #f))))

(define (rep v equiv-vars)
  (cond [(null? equiv-vars) v]
        [(memv v (car equiv-vars)) (caar equiv-vars)]
        [else (rep v (cdr equiv-vars))]))

(define (merge vars refs reps rhs unsolved solved equiv-to-v equiv-vars)
  (if (null? vars)
      (let ([e (eqn (car reps) refs rhs)]
            [equiv-vars (if (and (null? equiv-to-v) (null? (cdr reps)))
                            equiv-vars
                            (cons (append reps equiv-to-v) equiv-vars))])
        (if (or (null? rhs) (null? (cdr rhs)))
            (values (var (car reps)) unsolved (cons e solved) equiv-vars)
            (values (var (car reps)) (cons e unsolved) solved equiv-vars)))
      (let* ([v (rep (var-num (car vars)) equiv-vars)]
             [e-from-u (assv v unsolved)]
             [e-from-s (and (not e-from-u) (assv v solved))]
             [e (or e-from-u e-from-s (eqn v 1 '()))])
        (merge (cdr vars)
               (- (+ refs (eqn-count e)) 1)
               (if (memv v reps) reps (cons v reps))
               (append rhs (eqn-rhs e))
               (if e-from-u (remove e-from-u unsolved) unsolved)
               (if e-from-s (remove e-from-s solved) solved)
               (append equiv-to-v (cdr (or (assv v equiv-vars) (list #f))))
               (remove (assv v equiv-vars) equiv-vars)))))

(define (factor terms u s equiv-vars)
  (let ([vars (filter var? terms)])
    (if (null? vars)
        (let ([head (common-prefix (prefix (car terms)) (cdr terms))])
          (case head
            [(#t)
             (let*-values
               ([(car-c u~ s~ equiv-vars~) (factor (map car terms) u  s  equiv-vars)]
                [(cdr-c u~ s~ equiv-vars~) (factor (map cdr terms) u~ s~ equiv-vars~)])
               (values (and car-c cdr-c (cons car-c cdr-c)) u~ s~ equiv-vars~))]
            [else (values head u s equiv-vars)]))
        (merge vars 0 '() (filter (lambda (x) (not (var? x))) terms) u s '() equiv-vars))))

(define (merge-vars l1 l2 accum)
  (cond [(null? l1) (append accum l2)]
        [(null? l2) (append accum l1)]
        [(< (caar l1) (caar l2)) (merge-vars (cdr l1) l2 (append accum (list (car l1))))]
        [(> (caar l1) (caar l2)) (merge-vars l1 (cdr l2) (append accum (list (car l2))))]
        [else (merge-vars (cdr l1) (cdr l2)
                          (append accum (list (cons (caar l1) (+ (cdar l1) (cdar l2))))))]))

(define (vars-in t)
  (cond [(null? t) '()]
        [(var? t) `((,(var-num t) . ,1))]
        [(pair? t) (merge-vars (vars-in (car t)) (vars-in (cdr t)) '())]
        [else '()]))

; An invariant of solved sets of eqns for this algorithm is any variable in
; the lhs of an equation is only referred to by an equation after it, not before

(define (solve unsolved solved vars-in-solved equiv-vars)
  (if (null? unsolved) (values solved equiv-vars)
      (let ([e (find (lambda (x) (eqv? 0 (eqn-count x))) unsolved)])
        (if e
            (let* ([v (eqn-var e)]
                   [rhs (eqn-rhs e)]
                   [new-count (let ([n (assv v vars-in-solved)]) (if n (cdr n) 0))])
              (if (null? rhs)
                  (solve (remove e unsolved)
                         (cons (eqn v new-count rhs) solved)
                         vars-in-solved
                         equiv-vars)
                  (let-values ([(c eqns~ equiv-vars~)
                                (factor rhs (remove e unsolved) equiv-vars)])
                    (if c
                        (solve eqns~
                               (cons (eqn v new-count (list c)) solved)
                               (merge-vars (vars-in c) vars-in-solved '())
                               equiv-vars~)
                        (values #f '())))))
            (values #f '())))))

(define (unify t1 t2 solved equiv-vars)
  (let-values ([(c unsolved solved equiv-vars)
                (factor `(,t1 ,t2) '() solved equiv-vars)])
    (solve unsolved solved equiv-vars)))

(define (unifyold t1 t2 solved equiv-vars)
  (define (initialize eqns var-counts)
    (if (null? var-counts)
        (let-values ([(c eqns~ equiv-vars~) (factor `(,t1 ,t2) eqns equiv-vars)])
          (solve eqns~ solved equiv-vars~))
        (let* ([vc (car var-counts)]
               [v (rep (car vc) equiv-vars)]
               [e (assv v solved)])
          (initialize
            (cons
              (if e (eqn v (+ (cdr vc) (eqn-count e)) (eqn-rhs e)) (eqn v (cdr vc) '()))
              eqns)
            (cdr var-counts)))))
  (initialize '() (merge-vars (vars-in t1) (vars-in t2) '())))

(trace unify)
