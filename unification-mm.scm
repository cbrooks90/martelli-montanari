(define var vector)
(define var? vector?)
(define (var-num x) (vector-ref x 0))
(define (var=? v1 v2) (= (var-num v1) (var-num v2)))

(define (multieqn c vs ts) `(,c ,vs . ,ts))
(define multieqn-count car)
(define multieqn-vars cadr)
(define multieqn-rhs cddr)

(define (prefix term)
  (cond [(null? term) '()]
        [(pair? term) #t]; Something to distinguish this case from a symbol prefix
        [(integer? term) term]
        [(symbol? term) term]
        [else (error 'prefix "Unrecognized term")]))

(define (common-prefix terms)
  (let loop ([terms (cdr terms)]
             [last (prefix (car terms))])
    (cond [(null? terms) last]
          [(eqv? last (prefix (car terms))) (loop (cdr terms) last)]
          [else #f])))

(define (find-equation var eqns)
  (let loop ([eqns eqns])
    (cond [(null? eqns) #f]
          [(eqv? var (multieqn-vars (car eqns))) (car eqns)]
          [else (loop (cdr eqns))])))

(define (rep v equiv-vars)
  (let loop ([vs equiv-vars])
    (cond [(null? vs) v]
          [(memv v vs) (car vs)]
          [else (loop (cdr vs))])))

(define (canonicalize vars rhs eqns equiv-vars)
  (let loop ([vars vars]
             [refs 0]
             [reps '()]
             [rhs rhs]
             [eqns eqns]
             [equiv-to-v '()]
             [equiv-vars equiv-vars])
    (if (null? vars)
        (values (var (car reps))
                (if (eqv? (car reps) 'aux) eqns (cons (multieqn refs (car reps) rhs) eqns))
                (cons (append reps equiv-to-v) equiv-vars))
        (let* ([v (rep (var-num (car vars)) equiv-vars)]
               [eqn (find-equation v eqns)])
          (loop (cdr vars)
                (- (+ refs (multieqn-count eqn)) 1)
                (if (memv v reps) reps (cons v reps))
                (append rhs (multieqn-rhs eqn))
                (remove eqn eqns)
                (append equiv-to-v (cdr (or (assv v equiv-vars) (list #f))))
                (remove (assv v equiv-vars) equiv-vars))))))

(define (factor terms eqns equiv-vars)
  (let ([vars (filter var? terms)])
    (if (null? vars)
        (let ([head (common-prefix terms)])
          (case head
            [(#t)
             (let*-values
               ([(car-c eqns~ equiv-vars~) (factor (map car terms) eqns equiv-vars)]
                [(cdr-c eqns~ equiv-vars~) (factor (map cdr terms) eqns~ equiv-vars~)])
               (values (and car-c cdr-c (cons car-c cdr-c)) eqns~ equiv-vars~))]
            [else (values head eqns equiv-vars)]))
        (canonicalize vars (filter (lambda (x) (not (var? x))) terms) eqns equiv-vars))))

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

; An invariant of solved sets of multieqns for this algorithm is any variable in
; the lhs of an equation is only referred to by an equation after it, not before

(define (solve unsolved solved vars-in-solved equiv-vars)
  (if (null? unsolved) (values solved equiv-vars)
      (let ([meqn (assv 0 unsolved)])
        (and meqn (let* ([vars (multieqn-vars meqn)]
                         [rhs (multieqn-rhs meqn)]
                         [new-count (fold-left
                                      (lambda (y x)
                                        (if (memv (car x) vars)
                                            (+ (cdr x) y) y))
                                      0 vars-in-solved)])
                    (if (null? rhs)
                        (solve (remove meqn unsolved)
                               (cons (multieqn new-count vars rhs) solved)
                               vars-in-solved
                               equiv-vars)
                        (let-values ([(c eqns~ equiv-vars~)
                                      (factor rhs (remove meqn unsolved) equiv-vars)])
                          (and c (if (eqv? vars 'aux)
                                     (solve eqns~ solved vars-in-solved equiv-vars~)
                                     (solve eqns~
                                            (cons (multieqn new-count vars (list c)) solved)
                                            (merge-vars (vars-in c) vars-in-solved '())
                                            equiv-vars~))))))))))

(define (initialize-eqns eqns var-counts equiv-vars)
  (if (null? var-counts) eqns
      (let ([v (rep (caar var-counts) equiv-vars)])
        (let loop ([eqns eqns] [accum '()])
          (cond [(null? eqns)
                 (initialize-eqns
                   (cons (multieqn (cdar var-counts) v '()) accum)
                   (cdr var-counts)
                   equiv-vars)]
                [(eqv? v (multieqn-vars (car eqns)))
                 (initialize-eqns
                   (append accum
                           (multieqn (+ (cdar var-counts) (multieqn-count (car eqn)))
                                     v
                                     (multieqn-rhs (car eqn)))
                           (cdr eqns))
                   (cdr var-counts)
                   equiv-vars)]
                [else (loop (cdr eqns) (cons (car eqns) accum))])))))

(define (unify t1 t2 prev-solved equiv-vars)
  (let ([var-counts (merge-vars (vars-in t1) (vars-in t2) '())])
    (solve (cons
             (multieqn 0 'aux (list t1 t2))
             (initialize-eqns prev-solved var-counts equiv-vars))
           '() '() '())))

(trace unify rep find-equation initialize-eqns solve factor canonicalize)
(unify `(,(var 0) ,(var 1)) `(5 7) '() '())
