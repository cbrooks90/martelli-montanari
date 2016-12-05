(load "miniKanren-wrappers.scm")
(load "microKanren-test-programs.scm")

(define (reify-1st~ s/c)
  (let ((v (walk* (var 0) (car s/c))))
    (walk* v (reify-s~ v '()))))

(define (walk u s)
  (define (var=? v1 v2) (= (var-num v1) (var-num v2)))
  (let ((pr (and (var? u) (assp (lambda (v) (var=? u v)) s))))
    (if pr (walk (cdr pr) s) u)))

(define (walk* v s)
  (let ((v (walk v s)))
    (cond
      ((var? v) v)
      ((pair? v) (cons (walk* (car v) s)
                       (walk* (cdr v) s)))
      (else  v))))

(define (reify-s~ v s)
  (let ((v (walk v s)))
    (cond
      ((var? v)
       (let  ((n (reify-name~ (length s))))
         (cons `(,v . ,n) s)))
      ((pair? v) (reify-s~ (cdr v) (reify-s~ (car v) s)))
      (else s))))

(define (reify-name~ n)
  (string->symbol
    (string-append "_" "." (number->string n))))

(test-check
  "second-set t1"
  (let (($ ((call/fresh (lambda (q) (== q 5))) empty-state)))
    (reify-1st (car $)))
  (reify-1st~ '(((#(0) . 5)) . 1)))

(test-check
  "second-set t2"
  (let (($ ((call/fresh (lambda (q) (== q 5))) empty-state)))
    (cdr $))
  '())

(test-check
  "second-set t3"
  (let (($ (a-and-b empty-state)))
    (reify-1st (car $)))
  (reify-1st~ '(((#(1) . 5) (#(0) . 7)) . 2)))

(test-check
  "second-set t3, take"
  (let (($ (a-and-b empty-state)))
    (map reify-1st (take 1 $)))
  (map reify-1st~ '((((#(1) . 5) (#(0) . 7)) . 2))))

(test-check
  "second-set t4"
  (let (($ (a-and-b empty-state)))
    (reify-1st (car (cdr $))))
  (reify-1st~ '(((#(1) . 6) (#(0) . 7)) . 2)))

(test-check
  "second-set t5"
  (let (($ (a-and-b empty-state)))
    (cdr (cdr $)))
  '())

(test-check
  "who cares"
  (let (($ ((call/fresh (lambda (q) (fives q))) empty-state)))
    (map reify-1st (take 1 $)))
  (map reify-1st~ '((((#(0) . 5)) . 1))))

(test-check
  "take 2 a-and-b stream"
  (let (($ (a-and-b empty-state)))
    (map reify-1st (take 2 $)))
  (map reify-1st~ '((((#(1) . 5) (#(0) . 7)) . 2)
                    (((#(1) . 6) (#(0) . 7)) . 2))))

(test-check
  "take-all a-and-b stream"
  (let (($ (a-and-b empty-state)))
    (map reify-1st (take-all $)))
  (map reify-1st~ '((((#(1) . 5) (#(0) . 7)) . 2)
                    (((#(1) . 6) (#(0) . 7)) . 2))))

(test-check
  "ground appendo"
  (reify-1st (car ((ground-appendo empty-state))))
  (reify-1st~ '(((#(2) b) (#(1)) (#(0) . a)) . 3)))

(test-check
  "ground appendo2"
  (reify-1st (car ((ground-appendo2 empty-state))))
  (reify-1st~ '(((#(2) b) (#(1)) (#(0) . a)) . 3)))

(test-check
  "appendo"
  (map reify-1st (take 2 (call-appendo empty-state)))
  (map reify-1st~ '((((#(0) #(1) #(2) #(3)) (#(2) . #(3)) (#(1))) . 4)
                    (((#(0) #(1) #(2) #(3)) (#(2) . #(6)) (#(5)) (#(3) #(4) . #(6)) (#(1) #(4) . #(5))) . 7))))

(test-check
  "appendo2"
  (map reify-1st (take 2 (call-appendo2 empty-state)))
  (map reify-1st~ '((((#(0) #(1) #(2) #(3)) (#(2) . #(3)) (#(1))) . 4)
                    (((#(0) #(1) #(2) #(3)) (#(3) #(4) . #(6))
                      (#(2) . #(6)) (#(5)) (#(1) #(4) . #(5))) . 7))))

(test-check
  "reify-1st across appendo"
  (map reify-1st (take 2 (call-appendo empty-state)))
  '((() _.0 _.0) ((_.0) _.1 (_.0 . _.1))))

(test-check
  "reify-1st across appendo2"
  (map reify-1st (take 2 (call-appendo2 empty-state)))
  '((() _.0 _.0) ((_.0) _.1 (_.0 . _.1))))

(test-check
  "many non-ans"
  (map reify-1st (take 1 (many-non-ans empty-state)))
  (map reify-1st~ '((((#(0) . 3)) . 1))))
