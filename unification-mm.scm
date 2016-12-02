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
        [else (rep (cdr equiv-vars))]))

(define (merge vars refs reps rhs eqns equiv-to-v equiv-vars)
  (if (null? vars)
      (values (var (car reps))
              (if (eqv? (car reps) 'aux) eqns (cons (eqn (car reps) refs rhs) eqns))
              (if (and (null? equiv-to-v) (null? (cdr reps)))
                  equiv-vars
                  (cons (append reps equiv-to-v) equiv-vars)))
      (let* ([v (rep (var-num (car vars)) equiv-vars)]
             [eqn (assv v eqns)])
        (merge (cdr vars)
               (- (+ refs (eqn-count eqn)) 1)
               (if (memv v reps) reps (cons v reps))
               (append rhs (eqn-rhs eqn))
               (remove eqn eqns)
               (append equiv-to-v (cdr (or (assv v equiv-vars) (list #f))))
               (remove (assv v equiv-vars) equiv-vars)))))

(define (factor terms eqns equiv-vars)
  (let ([vars (filter var? terms)])
    (if (null? vars)
        (let ([head (common-prefix (prefix (car terms)) (cdr terms))])
          (case head
            [(#t)
             (let*-values
               ([(car-c eqns~ equiv-vars~) (factor (map car terms) eqns equiv-vars)]
                [(cdr-c eqns~ equiv-vars~) (factor (map cdr terms) eqns~ equiv-vars~)])
               (values (and car-c cdr-c (cons car-c cdr-c)) eqns~ equiv-vars~))]
            [else (values head eqns equiv-vars)]))
        (merge vars 0 '() (filter (lambda (x) (not (var? x))) terms) eqns '() equiv-vars))))

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

(define (find-unreferenced eqns)
  (cond [(null? eqns) #f]
        [(eqv? 0 (eqn-count (car eqns))) (car eqns)]
        [else (find-unreferenced (cdr eqns))]))

; An invariant of solved sets of eqns for this algorithm is any variable in
; the lhs of an equation is only referred to by an equation after it, not before

(define (solve unsolved solved vars-in-solved equiv-vars)
  (if (null? unsolved) (values solved equiv-vars)
      (let ([e (find-unreferenced unsolved)])
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
                    (cond [(and c (eqv? v 'aux))
                           (solve eqns~ solved vars-in-solved equiv-vars~)]
                          [c (solve eqns~
                                    (cons (eqn v new-count (list c)) solved)
                                    (merge-vars (vars-in c) vars-in-solved '())
                                    equiv-vars~)]
                          [else (values #f '())]))))
            (values #f '())))))

; Maybe this can be done with merge
(define (initialize-eqns eqns var-counts equiv-vars)
  (if (null? var-counts) eqns
      (let ([v (rep (caar var-counts) equiv-vars)])
        (let loop ([eqns eqns] [accum '()])
          (cond [(null? eqns)
                 (initialize-eqns
                   (cons (eqn v (cdar var-counts) '()) accum)
                   (cdr var-counts)
                   equiv-vars)]
                [(eqv? v (eqn-var (car eqns)))
                 (initialize-eqns
                   (append
                     accum
                     (cons
                       (eqn v (+ (cdar var-counts) (eqn-count (car eqns))) (eqn-rhs (car eqns)))
                       (cdr eqns)))
                   (cdr var-counts)
                   equiv-vars)]
                [else (loop (cdr eqns) (cons (car eqns) accum))])))))

(define (unify t1 t2 prev-solved equiv-vars)
  (let ([var-counts (merge-vars (vars-in t1) (vars-in t2) '())])
    (solve (cons
             (eqn 'aux 0 (list t1 t2))
             (initialize-eqns prev-solved var-counts equiv-vars))
           '() '() equiv-vars)))
