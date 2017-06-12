(define var vector)
(define var? vector?)
(define (var=? v1 v2) (eqv? (vector-ref v1 0) (vector-ref v2 0)))

; By 'equation' we mean an equivalence class of equations, each of which
; associates a set of variables to a set of terms which are not variables
(define (eqn v vs c ts) `(,v ,vs ,c . ,ts))
(define eqn-var car)
(define eqn-vars cadr)
(define eqn-count caddr)
(define eqn-terms cdddr)

;; Helper functions

(define (find-class v li)
  (find (lambda (e) (or (var=? v (eqn-var e)) (member v (eqn-vars e)))) li))

(define (merge-vars l1 l2)
  (if (null? l1) l2
      (merge-vars
        (cdr l1)
        (let loop ([v (car l1)] [l2 l2])
          (cond [(null? l2) (list v)]
                [(var=? (car v) (caar l2))
                 (cons (cons (car v) (+ (cdr v) (cdar l2))) (cdr l2))]
                [else (cons (car l2) (loop v (cdr l2)))])))))

(define (vars-in t)
  (cond [(null? t) '()]
        [(var? t) `((,t . ,1))]
        [(pair? t) (merge-vars (vars-in (car t)) (vars-in (cdr t)))]
        [else '()]))

(define (mod-refs li vars proc)
  (let loop ([li li] [vars vars] [proc proc])
    (if (null? li) '()
        (let ([e (car li)])
          (cons (eqn (eqn-var e) (eqn-vars e)
                     (fold-left
                       (lambda (acc x)
                         (if (member (car x) (cons (eqn-var e) (eqn-vars e)))
                             (proc acc (cdr x))
                             acc))
                       (eqn-count e)
                       vars)
                     (eqn-terms e)) (loop (cdr li) vars proc))))))

(define (prefix term)
  (cond [(pair? term) 'pair]
        [(symbol? term) (list term)]
        [(not term) 'false]
        [else term]))

(define (unprefix p)
  (cond [(pair? p) (car p)]
        [(eq? p 'false) #f]
        [else p]))

(define (common-prefix first rest)
  (if (null? rest) first
      (let ([p (prefix (car rest))])
        (if (equal? first p)
            (common-prefix p (cdr rest))
            #f))))

(define (filter-vars ts)
  (if (null? ts) (values '() '())
      (let-values ([(vars not-vars) (filter-vars (cdr ts))])
        (if (var? (car ts))
            (values (cons (car ts) vars) not-vars)
            (values vars (cons (car ts) not-vars))))))

;; The Martelli-Montanari algorithm

(define (find-class* v u s)
  (let ([u-eqn (find-class v u)])
    (if u-eqn (values u-eqn #t #f)
        (let ([s-eqn (find-class v s)])
          (if s-eqn (values s-eqn #f #t)
              (values (eqn v '() 0 '()) #f #f))))))

(define (union vars u s u-vars s-vars var~ vars~ count~ ts~)
  (if (null? vars)
      (let ([e (eqn var~ (remove var~ vars~) count~ ts~)])
        (if (or (null? ts~) (null? (cdr ts~)))
            (values var~ (mod-refs u u-vars -) (cons e (mod-refs s u-vars -)))
            (values var~ (cons e (mod-refs u s-vars +)) (mod-refs s s-vars +))))
      (let-values ([(e e-u? e-s?) (find-class* (car vars) u s)])
        (union (cdr vars)
               (if e-u? (remove e u) u)
               (if e-s? (remove e s) s)
               (if e-u? (merge-vars (vars-in (eqn-terms e)) u-vars) u-vars)
               (if e-s? (merge-vars (vars-in (eqn-terms e)) s-vars) s-vars)
               (eqn-var e)
               (cons (eqn-var e) (append (eqn-vars e) vars~))
               (- (+ count~ (eqn-count e)) 1)
               (append (eqn-terms e) ts~)))))

(define (disagree terms u s)
  (let-values ([(vars not-vars) (filter-vars terms)])
    (if (null? vars)
        (let ([head (common-prefix (prefix (car terms)) (cdr terms))])
          (case head
            [(pair)
             (let-values ([(car-c u s) (disagree (map car terms) u s)])
               (let-values ([(cdr-c u s)
                             (if s (disagree (map cdr terms) u s)
                                 (values #f '() #f))])
                 (values (cons car-c cdr-c) u s)))]
            [(#f) (values head u #f)]
            [else (values (unprefix head) u s)]))
        (union vars u s (vars-in not-vars) '() #f '() 0 not-vars))))

(define (unify to-do s)
  (if (null? to-do) s
      (let ([e (find (lambda (x) (= 0 (eqn-count x))) to-do)])
        (and e
             (let-values ([(c to-do s) (disagree (eqn-terms e) (remove e to-do) s)])
               (and s
                    (unify to-do (if (eqn-var e)
                                     (cons (eqn (eqn-var e) (eqn-vars e) 0 (list c)) s)
                                     s))))))))
