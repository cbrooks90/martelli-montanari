(define (walk u s)
  (let ((pr (and (var? u) (assp (lambda (v) (var=? u v)) s))))
    (if pr (walk (cdr pr) s) u)))

(define (occurs-check x v s)
  (let ((v (walk v s)))
    (cond
      ((var? v) (eq? v x))
      ((pair? v)
       (or
         (occurs-check x (car v) s)
         (occurs-check x (cdr v) s)))
      (else #f))))

(define (ext-s x v s)
  (cond
    ((occurs-check x v s) #f)
    (else (cons `(,x . ,v) s))))

(define (old-unify u v s)
  (let ((u (walk u s)) (v (walk v s)))
    (cond
      ((and (var? u) (var? v) (var=? u v)) s)
      ((var? u) (ext-s u v s))
      ((var? v) (ext-s v u s))
      ((and (pair? u) (pair? v))
       (let ((s (old-unify (car u) (car v) s)))
         (and s (old-unify (cdr u) (cdr v) s))))
      ((equal? u v) s)
      (else #f))))

;; Reification

(define (old-reify-1st s/c)
  (let ((v (walk* (var 0) (cadr s/c))))
    (walk* v (old-reify-s v '()))))

(define (walk* v s)
  (let ((v (walk v s)))
    (cond
      ((var? v) v)
      ((pair? v) (cons (walk* (car v) s)
                       (walk* (cdr v) s)))
      (else  v))))

(define (old-reify-s v s)
  (let ((v (walk v s)))
    (cond
      ((var? v)
       (let  ((n (old-reify-name (length s))))
         (cons `(,v . ,n) s)))
      ((pair? v) (old-reify-s (cdr v) (old-reify-s (car v) s)))
      (else s))))

(define (old-reify-name n)
  (string->symbol
    (string-append "_" "." (number->string n))))
