(load "microKanren.scm")

(define-syntax Zzz
  (syntax-rules ()
    ((_ g) (lambda (s/c) (lambda () (g s/c))))))

(define-syntax conj+
  (syntax-rules ()
    ((_ g) (Zzz g))
    ((_ g0 g ...) (conj (Zzz g0) (conj+ g ...)))))

(define-syntax disj+
  (syntax-rules ()
    ((_ g) (Zzz g))
    ((_ g0 g ...) (disj (Zzz g0) (disj+ g ...)))))

(define-syntax fresh
  (syntax-rules ()
    ((_ () g0 g ...) (conj+ g0 g ...))
    ((_ (x0 x ...) g0 g ...)
     (call/fresh
      (lambda (x0)
        (fresh (x ...) g0 g ...))))))

(define-syntax conde
  (syntax-rules ()
    ((_ (g0 g ...) ...) (disj+ (conj+ g0 g ...) ...))))

(define-syntax run
  (syntax-rules ()
    ((_ n (x ...) g0 g ...)
     (map reify-1st (take n (call/goal (fresh (x ...) g0 g ...)))))))

(define-syntax run*
  (syntax-rules ()
    ((_ (x ...) g0 g ...)
     (map reify-1st (take-all (call/goal (fresh (x ...) g0 g ...)))))))

(define empty-state '(() . 0))

(define (call/goal g) (g empty-state))

(define (pull $)
  (if (procedure? $) (pull ($)) $))

(define (take-all $)
  (let (($ (pull $)))
    (if (null? $) '() (cons (car $) (take-all (cdr $))))))

(define (take n $)
  (if (zero? n) '()
      (let (($ (pull $)))
        (if (null? $) '() (cons (car $) (take (- n 1) (cdr $)))))))

(define (reify-1st s/c)
  (let ((v (resolve* (var 0) (car s/c))))
    (let-values ([(res _) (reify-s v '())])
      res)))

(define (resolve u s)
  (let ([pr (and (var? u)
                 (let loop ([s s])
                   (cond [(null? s) #f]
                         [(memv (var-num u) (multieqn-vars (car s))) (car s)]
                         [else (loop (cdr s))])))])
    (cond [(and pr (null? (multieqn-rhs pr))) (var (car (multieqn-vars pr)))]
          [pr (resolve (car (multieqn-rhs pr)) s)]
          [else u])))

(define (resolve* v s)
  (let ([v (resolve v s)])
    (cond
      ((var? v) v)
      ((pair? v) (cons (resolve* (car v) s)
                       (resolve* (cdr v) s)))
      (else v))))

(define (reify-s v s)
  (cond [(var? v)
         (let ([binding (assv (var-num v) s)])
           (if binding
               (values (cdr binding) s)
               (let ([new-binding (reify-name (length s))])
                 (values new-binding (cons (cons (var-num v) new-binding) s)))))]
        [(pair? v)
         (let*-values ([(car-v car-b) (reify-s (car v) s)]
                       [(cdr-v cdr-b) (reify-s (cdr v) car-b)])
           (values (cons car-v cdr-v) cdr-b))]
        [else (values v s)]))

(define (reify-name n)
  (string->symbol
   (string-append "_." (number->string n))))

(define (fresh/nf n f)
  (letrec
      ((app-f/v*
        (lambda (n v*)
          (cond
            ((zero? n) (apply f (reverse v*)))
            (else (call/fresh
                   (lambda (x)
                     (app-f/v* (- n 1) (cons x v*)))))))))
    (app-f/v* n '())))

;;; Test programs

(define-syntax test-check
  (syntax-rules ()
    ((_ title tested-expression expected-result)
     (begin
       (printf "Testing ~s\n" title)
       (let* ((expected expected-result)
              (produced tested-expression))
         (or (equal? expected produced)
             (errorf 'test-check
                     "Failed: ~a~%Expected: ~a~%Computed: ~a~%"
                     'tested-expression expected produced)))))))

(define (appendo l s out)
  (conde
   ((== '() l) (== s out))
   ((fresh
     (a d res)
     (== `(,a . ,d) l)
     (== `(,a . ,res) out)
     (appendo d s res)))))

(test-check
 'run*
 (run* (q) (fresh (x y) (== `(,x ,y) q) (appendo x y '(1 2 3 4 5))))
 '((() (1 2 3 4 5))
   ((1) (2 3 4 5))
   ((1 2) (3 4 5))
   ((1 2 3) (4 5))
   ((1 2 3 4) (5))
   ((1 2 3 4 5) ())))

(test-check
 'run*2
 (run* (q x y) (== `(,x ,y) q) (appendo x y '(1 2 3 4 5)))
 '((() (1 2 3 4 5))
   ((1) (2 3 4 5))
   ((1 2) (3 4 5))
   ((1 2 3) (4 5))
   ((1 2 3 4) (5))
   ((1 2 3 4 5) ())))

(test-check
 'rember*o
 (letrec
     ((rember*o
       (lambda (tr o)
         (conde
          ((== '() tr) (== '() o))
          ((fresh
            (a d)
            (== `(,a . ,d) tr)
            (conde
             ((fresh
               (aa da)
               (== `(,aa . ,da) a)
               (fresh
                (a^ d^)
                (rember*o a a^)
                (rember*o d d^)
                (== `(,a^ . ,d^) o))))
             ((== a 8) (rember*o d o))
             ((fresh
               (d^)
               (rember*o d d^)
               (== `(,a . ,d^) o))))))))))
   (run 8 (q) (rember*o q '(1 2 8 3 4 5))))
 '((1 2 8 3 4 5)
   (1 2 8 3 4 5 8)
   (1 2 8 3 4 8 5)
   (1 2 8 3 8 4 5)
   (1 2 8 8 3 4 5)
   (1 2 8 8 3 4 5)
   (1 8 2 8 3 4 5)
   (8 1 2 8 3 4 5)))

(test-check
 'rember*o
 (letrec
     ((rember*o
       (lambda (tr o)
         (conde
          ((== '() tr) (== '() o))
          ((fresh
            (a d)
            (== `(,a . ,d) tr)
            (conde
             ((fresh
               (aa da)
               (== `(,aa . ,da) a)
               (fresh
                (a^ d^)
                (== `(,a^ . ,d^) o)
                (rember*o d d^)
                (rember*o a a^))))
             ((== a 8) (rember*o d o))
             ((fresh
               (d^)
               (== `(,a . ,d^) o)
               (rember*o d d^))))))))))
   (run 9 (q) (rember*o q '(1 (2 8 3 4) 5))))
 '((1 (2 8 3 4) 5)
   (1 (2 8 3 4) 5 8)
   (1 (2 8 3 4) 5 8 8)
   (1 (2 8 3 4) 8 5)
   (1 8 (2 8 3 4) 5)
   (8 1 (2 8 3 4) 5)
   (1 (2 8 3 4) 5 8 8 8)
   (1 (2 8 3 4) 5 8 8 8 8)
   (1 (2 8 3 4) 5 8 8 8 8 8)))
