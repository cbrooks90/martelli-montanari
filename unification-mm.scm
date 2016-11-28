(define var vector)
(define var? vector?)
(define (var-num x) (vector-ref x 0))
(define (var=? v1 v2) (= (var-num v1) (var-num v2)))

(define (multieqn c vs ts) `(,c ,vs . ,ts))
(define multieqn-count car)
(define multieqn-vars cadr)
(define multieqn-rhs cddr)

(define (sorted-union l1 l2 accum)
  (cond [(null? l1) (append accum l2)]
        [(null? l2) (append accum l1)]
        [(< (car l1) (car l2)) (sorted-union (cdr l1) l2 (append accum (list (car l1))))]
        [(> (car l1) (car l2)) (sorted-union l1 (cdr l2) (append accum (list (car l2))))]
        [else (sorted-union (cdr l1) (cdr l2) (append accum (list (car l1))))]))

; The possible returns here should correspond to the dispatch cases in "factor"
(define (prefix term)
  (cond [(null? term) '()]
        [(pair? term) #t]; This is bad, should use 'cons but 'cons could be a user symbol
        [(integer? term) term]
        [(symbol? term) term]
        [else (error 'prefix "Unrecognized term")]))

(define (common-prefix terms)
  (let loop ([terms (cdr terms)]
             [last (prefix (car terms))])
    (cond [(null? terms) last]
          [(eqv? last (prefix (car terms))) (loop (cdr terms) last)]
          [else #f])))

(define (factor terms)
  (let ([vars (filter var? terms)])
    (if (null? vars)
        (let ([head (common-prefix terms)])
          (case head
            [(#t)
             (let-values ([(car-c car-f) (factor (map car terms))]
                          [(cdr-c cdr-f) (factor (map cdr terms))])
               (values (and car-c cdr-c (cons car-c cdr-c))
                       (append car-f cdr-f)))]
            [else (values head '())]))
        (values (car vars)
                (list (multieqn (- (length vars))
                                (sort < (map var-num vars))
                                (filter (lambda (x) (not (var? x))) terms)))))))

(define (empty-intersect? l1 l2)
  (cond [(or (null? l1) (null? l2)) #t]
        [(< (car l1) (car l2)) (empty-intersect? (cdr l1) l2)]
        [(> (car l1) (car l2)) (empty-intersect? l1 (cdr l2))]
        [else #f]))

(define (merge-eqn e es)
  (let loop ([es es]
             [meets '()]
             [new-count (multieqn-count e)]
             [new-vars (multieqn-vars e)]
             [new-rhs (multieqn-rhs e)])
    (cond [(null? es) (values meets (multieqn new-count new-vars new-rhs))]
          [(empty-intersect? (multieqn-vars e) (multieqn-vars (car es)))
           (loop (cdr es) meets new-count new-vars new-rhs)]
          [else (loop (cdr es)
                      (cons (car es) meets)
                      (+ (multieqn-count (car es)) new-count)
                      (sorted-union new-vars (multieqn-vars (car es)) '())
                      (append new-rhs (multieqn-rhs (car es))))])))

; For every eqn x in l1, collect all variables and rhs's from equations whose
; vars have nonempty intersection with vars in x. Make a new equation from those
; vars+rhs's, then delete the originals
(define (compact-union l1 l2)
  (fold-left
    (lambda (y x)
      (let-values ([(touched-eqns new-eqn) (merge-eqn x y)])
        (cons new-eqn (fold-left (lambda (y x) (remove x y)) y touched-eqns))))
    l2 l1))

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

(define (solve unsolved solved vars-in-solved)
  (if (null? unsolved) solved
      (let ([meqn (assv 0 unsolved)])
        (and meqn (let* ([vars (multieqn-vars meqn)]
                         [rhs (multieqn-rhs meqn)]
                         [new-count (fold-left (lambda (y x) (if (memv (car x) vars)
                                                                 (+ (cdr x) y) y)) 0 vars-in-solved)])
                    (if (null? rhs)
                        (solve (remove meqn unsolved)
                               (cons (multieqn new-count vars rhs) solved)
                               vars-in-solved)
                        (let-values ([(c f) (factor rhs)])
                          (and c (if (eqv? (car vars) 'aux)
                                     (solve (compact-union f (remove meqn unsolved))
                                            solved vars-in-solved)
                                     (solve (compact-union f (remove meqn unsolved))
                                            (cons (multieqn new-count vars (list c)) solved)
                                            (merge-vars (vars-in c) vars-in-solved '())))))))))))

(define (unify t1 t2 s)
  (solve (cons
           (multieqn 0 `(aux) (list t1 t2))
           (compact-union
             (map (lambda (x) (multieqn (cdr x) (list (car x)) '()))
                  (merge-vars (vars-in t1) (vars-in t2) '()))
             s))
         '() '()))
