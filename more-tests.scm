(define succeed (== #f #f))
(define fail (== #f #t))
(define max-ticks 10000000)

(test-check "testc11.tex-1"
  (run* (q)
    fail)
  `())

(test-check "testc11.tex-2"
  (run* (q)
    (== #t q))
  `(#t))

(test-check "testc11.tex-3"
  (run* (q)
    fail
    (== #t q))
  `())

(define g fail)

(test-check "testc11.tex-4"
  (run* (q)
    succeed
    (== #t q))
  (list #t))

(test-check "testc11.tex-5"
  (run* (q)
    succeed
    (== #t q))
  `(#t))

(test-check "testc11.tex-6"
  (run* (r)
    succeed
    (== 'corn r))
  (list 'corn))

(test-check "testc11.tex-7"
  (run* (r)
    succeed
    (== 'corn r))
  `(corn))

(test-check "testc11.tex-8"
  (run* (r)
    fail
    (== 'corn r))
  `())

(test-check "testc11.tex-9"
  (run* (q)
    succeed
    (== #f q))
  `(#f))

(test-check "testc11.tex-10"
  (run* (x)
    (let ((x #f))
      (== #t x)))
  '())

(test-check "testc11.tex-11"
  (run* (q)
    (fresh (x)
      (== #t x)
      (== #t q)))
  (list #t))

(test-check "testc11.tex-12"
  (run* (q)
    (fresh (x)
      (== x #t)
      (== #t q)))
  (list #t))

(test-check "testc11.tex-13"
  (run* (q)
    (fresh (x)
      (== x #t)
      (== q #t)))
  (list #t))

(test-check "testc11.tex-14"
  (run* (x)
    succeed)
  (list `_.0))

(test-check "testc11.tex-15"
  (run* (x)
    (let ((x #f))
      (fresh (x)
        (== #t x))))
  `(_.0))

(test-check "testc11.tex-16"
  (run* (r)
    (fresh (x y)
      (== (cons x (cons y '())) r)))
  (list `(_.0 _.1)))

(test-check "testc11.tex-17"
  (run* (s)
    (fresh (t u)
      (== (cons t (cons u '())) s)))
  (list `(_.0 _.1)))

(test-check "testc11.tex-18"
  (run* (r)
    (fresh (x)
      (let ((y x))
        (fresh (x)
          (== (cons y (cons x (cons y '()))) r)))))
  (list `(_.0 _.1 _.0)))

(test-check "testc11.tex-19"
  (run* (r)
    (fresh (x)
      (let ((y x))
        (fresh (x)
          (== (cons x (cons y (cons x '()))) r)))))
  (list `(_.0 _.1 _.0)))

(test-check "testc11.tex-20"
  (run* (q)
    (== #f q)
    (== #t q))
  `())

(test-check "testc11.tex-21"
  (run* (q)
    (== #f q)
    (== #f q))
  '(#f))

(test-check "testc11.tex-22"
  (run* (q)
    (let ((x q))
      (== #t x)))
  (list #t))

(test-check "testc11.tex-23"
  (run* (r)
    (fresh (x)
      (== x r)))
  (list `_.0))

(test-check "testc11.tex-24"
  (run* (q)
    (fresh (x)
      (== #t x)
      (== x q)))
  (list #t))

(test-check "testc11.tex-25"
  (run* (q)
    (fresh (x)
      (== x q)
      (== #t x)))
  (list #t))

(test-check "testc11.tex-26"
  (run* (q)
    (fresh (x)
      (== (eq? x q) q)))
  (list #f))

(test-check "testc11.tex-27"
  (run* (q)
    (let ((x q))
      (fresh (q)
        (== (eq? x q) x))))
  (list #f))

(test-equal "testc11.tex-28"
  (cond
    (#f #t)
    (#t #f))
  #f)

(test-equal "testc11.tex-29"
  (cond
    (#f succeed)
    (#t fail))
  fail)

(test-check "testc13.tex-fail1"
  (run* (q)
    (conde
      (fail succeed)
      (succeed fail)))
  '())

(test-equal "testc13.tex-succeed1"
  (not (null? (run* (q)
                (conde
                  (fail fail)
                  (succeed succeed)))))
  #t)

(test-equal "testc13.tex-succeed2"
  (not (null? (run* (q)
                (conde
                  (succeed succeed)
                  (succeed fail)))))
  #t)

(test-check "testc11.tex-30"
  (run* (x)
    (conde
      ((== 'olive x) succeed)
      ((== 'oil x) succeed)))
  `(olive oil))

(test-check "testc11.tex-31"
  (run 1 (x)
    (conde
      ((== 'olive x) succeed)
      ((== 'oil x) succeed)))
  `(olive))

(test-check "testc11.tex-32"
  (run* (x)
    (conde
      ((== 'virgin x) fail)
      ((== 'olive x) succeed)
      (succeed succeed)
      ((== 'oil x) succeed)))
  `(olive _.0 oil))

(test-check "testc13.tex-conde1"
  (run* (x)
    (conde
      ((== 'olive x) succeed)
      (succeed succeed)
      ((== 'oil x) succeed)))
  `(olive _.0 oil))

(test-check "testc11.tex-33"
  (run 2 (x)
    (conde
      ((== 'extra x) succeed)
      ((== 'virgin x) fail)
      ((== 'olive x) succeed)
      ((== 'oil x) succeed)))
  `(extra olive))

(test-check "testc11.tex-34"
  (run* (r)
    (fresh (x y)
      (== 'split x)
      (== 'pea y)
      (== (cons x (cons y '())) r)))
  (list `(split pea)))

(test-check "testc11.tex-35"
  (run* (r)
    (fresh (x y)
      (conde
        ((== 'split x) (== 'pea y))
        ((== 'navy x) (== 'bean y)))
      (== (cons x (cons y '())) r)))
  `((split pea) (navy bean)))

(test-check "testc11.tex-36"
  (run* (r)
    (fresh (x y)
      (conde
        ((== 'split x) (== 'pea y))
        ((== 'navy x) (== 'bean y)))
      (== (cons x (cons y (cons 'soup '()))) r)))
  `((split pea soup) (navy bean soup)))

(define teacupo
  (lambda (x)
    (conde
      ((== 'tea x) succeed)
      ((== 'cup x) succeed))))

(test-check "testc11.tex-37"
  (run* (x)
    (teacupo x))
  `(tea cup))

(test-check "testc11.tex-38"
  (run* (r)
    (fresh (x y)
      (conde
        ((teacupo x) (== #t y) succeed)
        ((== #f x) (== #t y)))
      (== (cons x (cons y '())) r)))
  `((#f #t) (tea #t) (cup #t)))

(test-check "testc11.tex-39"
  (run* (r)
    (fresh (x y z)
      (conde
        ((== y x) (fresh (x) (== z x)))
        ((fresh (x) (== y x)) (== z x)))
      (== (cons y (cons z '())) r)))
  `((_.0 _.1) (_.0 _.1)))

(test-check "testc11.tex-40"
  (run* (r)
    (fresh (x y z)
      (conde
        ((== y x) (fresh (x) (== z x)))
        ((fresh (x) (== y x)) (== z x)))
      (== #f x)
      (== (cons y (cons z '())) r)))
  `((#f _.0) (_.0 #f)))

(test-check "testc11.tex-41"
  (run* (q)
    (let ((a (== #t q))
          (b (== #f q)))
      b))
  '(#f))

(test-check "testc11.tex-42"
  (run* (q)
    (let ((a (== #t q))
          (b (fresh (x)
               (== x q)
               (== #f x)))
          (c (conde
               ((== #t q) succeed)
               (succeed (== #f q)))))
      b))
  '(#f))

(test-equal "testc12.tex-1"
  (let ((x (lambda (a) a))
        (y 'c))
    (x y))
  'c)

(test-check "testc12.tex-2"
  (run* (r)
    (fresh (y x)
      (== `(,x ,y) r)))
  (list `(_.0 _.1)))

(test-check "testc12.tex-3"
  (run* (r)
    (fresh (v w)
      (== (let ((x v) (y w)) `(,x ,y)) r)))
  `((_.0 _.1)))

(test-equal "testc12.tex-4"
  (car `(grape raisin pear))
  `grape)

(test-equal "testc12.tex-5"
  (car `(a c o r n))
  'a)

(define caro
  (lambda (p a)
    (fresh (d)
      (== (cons a d) p))))

(test-check "testc12.tex-6"
  (run* (r)
    (caro `(a c o r n) r))
  (list 'a))

(test-equal "testc12.tex-7"
  'a
  (car `(a c o r n)))

(test-check "testc12.tex-8"
  (run* (q)
    (caro `(a c o r n) 'a)
    (== #t q))
  (list #t))

(test-equal "testc12.tex-9"
  'a
  (car `(a c o r n)))

(test-check "testc12.tex-10"
  (run* (r)
    (fresh (x y)
      (caro `(,r ,y) x)
      (== 'pear x)))
  (list 'pear))

(test-equal "testc12.tex-11"
  (cons
    (car `(grape raisin pear))
    (car `((a) (b) (c))))
  `(grape a))

(test-check "testc12.tex-12"
  (run* (r)
    (fresh (x y)
      (caro `(grape raisin pear) x)
      (caro `((a) (b) (c)) y)
      (== (cons x y) r)))
  (list `(grape a)))

(test-equal "testc12.tex-13"
  (cdr `(grape raisin pear))
  `(raisin pear))

(test-equal "testc12.tex-14"
  (car (cdr `(a c o r n)))
  'c)

(define cdro
  (lambda (p d)
    (fresh (a)
      (== (cons a d) p))))

(test-check "testc12.tex-15"
  (run* (r)
    (fresh (v)
      (cdro `(a c o r n) v)
      (caro v r)))
  (list 'c))

(test-equal "testc12.tex-16"
  (cons
    (cdr `(grape raisin pear))
    (car `((a) (b) (c))))
  `((raisin pear) a))

(test-check "testc12.tex-17"
  (run* (r)
    (fresh (x y)
      (cdro `(grape raisin pear) x)
      (caro `((a) (b) (c)) y)
      (== (cons x y) r)))
  (list `((raisin pear) a)))

(test-check "testc12.tex-18"
  (run* (q)
    (cdro '(a c o r n) '(c o r n))
    (== #t q))
  (list #t))

(test-equal "testc12.tex-19"
  `(c o r n)
  (cdr '(a c o r n)))

(test-check "testc12.tex-20"
  (run* (x)
    (cdro '(c o r n) `(,x r n)))
  (list 'o))

(test-equal "testc12.tex-21"
  `(o r n)
  (cdr `(c o r n)))

(test-check "testc12.tex-22"
  (run* (l)
    (fresh (x)
      (cdro l '(c o r n))
      (caro l x)
      (== 'a x)))
  (list `(a c o r n)))

(define conso
  (lambda (a d p)
    (== (cons a d) p)))

(test-check "testc12.tex-23"
  (run* (l)
    (conso '(a b c) '(d e) l))
  (list `((a b c) d e)))

(test-check "testc12.tex-24"
  (run* (x)
    (conso x '(a b c) '(d a b c)))
  (list 'd))

(test-equal "testc12.tex-25"
  (cons 'd '(a b c))
  `(d a b c))

(test-check "testc12.tex-26"
  (run* (r)
    (fresh (x y z)
      (== `(e a d ,x) r)
      (conso y `(a ,z c) r)))
  (list `(e a d c)))

(test-check "testc12.tex-27"
  (run* (x)
    (conso x `(a ,x c) `(d a ,x c)))
  (list 'd))

(define x 'd)

(test-equal "testc12.tex-28"
  (cons x `(a ,x c))
  `(d a ,x c))

(test-check "testc12.tex-29"
  (run* (l)
    (fresh (x)
      (== `(d a ,x c) l)
      (conso x `(a ,x c) l)))
  (list `(d a d c)))

(test-check "testc12.tex-30"
  (run* (l)
    (fresh (x)
      (conso x `(a ,x c) l)
      (== `(d a ,x c) l)))
  (list `(d a d c)))

(test-check "testc12.tex-31"
  (run* (l)
    (fresh (d x y w s)
      (conso w '(a n s) s)
      (cdro l s)
      (caro l x)
      (== 'b x)
      (cdro l d)
      (caro d y)
      (== 'e y)))
  (list `(b e a n s)))

(test-equal "testc12.tex-32"
  (null? `(grape raisin pear))
  #f)

(test-equal "testc12.tex-33"
  (null? '())
  #t)

(define nullo
  (lambda (x)
    (== '() x)))

(test-check "testc12.tex-34"
  (run* (q)
    (nullo `(grape raisin pear))
    (== #t q))
  `())

(test-check "testc12.tex-35"
  (run* (q)
    (nullo '())
    (== #t q))
  `(#t))

(test-check "testc12.tex-36"
  (run* (x)
    (nullo x))
  `(()))

(test-equal "testc12.tex-37"
  (eq? 'pear 'plum)
  #f)

(test-equal "testc12.tex-38"
  (eq? 'plum 'plum)
  #t)

(define eqo
  (lambda (x y)
    (== x y)))

(test-check "testc12.tex-39"
  (run* (q)
    (eqo 'pear 'plum)
    (== #t q))
  `())

(test-check "testc12.tex-40"
  (run* (q)
    (eqo 'plum 'plum)
    (== #t q))
  `(#t))

(test-equal "testc12.tex-41"
  (pair? `((split) . pea))
  #t)

(test-equal "testc12.tex-42"
  (pair? '())
  #f)

(test-equal "testc12.tex-43"
  (car `(pear))
  `pear)

(test-equal "testc12.tex-44"
  (cdr `(pear))
  `())

(test-equal "testc12.tex-45"
  (cons `(split) 'pea)
  `((split) . pea))

(test-check "testc12.tex-46"
  (run* (r)
    (fresh (x y)
      (== (cons x (cons y 'salad)) r)))
  (list `(_.0 _.1 . salad)))

(define pairo
  (lambda (p)
    (fresh (a d)
      (conso a d p))))

(test-check "testc12.tex-47"
  (run* (q)
    (pairo (cons q q))
    (== #t q))
  `(#t))

(test-check "testc12.tex-48"
  (run* (q)
    (pairo '())
    (== #t q))
  `())

(test-check "testc12.tex-49"
  (run* (q)
    (pairo 'pair)
    (== #t q))
  `())

(test-check "testc12.tex-50"
  (run* (x)
    (pairo x))
  (list `(_.0 . _.1)))

(test-check "testc12.tex-51"
  (run* (r)
    (pairo (cons r 'pear)))
  (list `_.0))

(define new-list?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((pair? l) (new-list? (cdr l)))
      (else #f))))

(test-equal "testc14.tex-1"
  (new-list? `((a) (a b) c))
  #t)

(test-equal "testc14.tex-2"
  (new-list? `())
  #t)

(test-equal "testc14.tex-3"
  (new-list? 's)
  #f)

(test-equal "testc14.tex-4"
  (new-list? `(d a t e . s))
  #f)

(define listo
  (lambda (l)
    (conde
      ((nullo l) succeed)
      ((pairo l)
       (fresh (d)
         (cdro l d)
         (listo d)))
      ((== #f #f) fail))))

(define listo
  (lambda (l)
    (conde
      ((nullo l) succeed)
      ((pairo l)
       (fresh (d)
         (cdro l d)
         (listo d)))
      (succeed fail))))

(define listo
  (lambda (l)
    (conde
      ((nullo l) succeed)
      ((pairo l)
       (fresh (d)
         (cdro l d)
         (listo d))))))

(test-check "testc14.tex-5"
  (run* (x)
    (listo `(a b ,x d)))
  (list `_.0))

(test-check "testc14.tex-6"
  (run 1 (x)
    (listo `(a b c . ,x)))
  (list `()))

(define e
  (make-engine
    (lambda ()
      (run* (x)
        (listo `(a b c . ,x))))))
(printf "Testing testc14.tex-7 (engine with ~s ticks fuel)\n" max-ticks)

(e max-ticks
   (lambda (t v) (error 'testc14.tex-7 "infinite loop returned ~s after ~s ticks" v (- max-ticks t)))
   (lambda (e^) (void)))

(test-check "testc14.tex-8"
  (run 5 (x)
    (listo `(a b c . ,x)))
  `(()
    (_.0)
    (_.0 _.1)
    (_.0 _.1 _.2)
    (_.0 _.1 _.2 _.3)))

(define lol?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((new-list? (car l)) (lol? (cdr l)))
      (else #f))))

(define lolo
  (lambda (l)
    (conde
      ((nullo l) succeed)
      ((fresh (a)
         (caro l a)
         (listo a))
       (fresh (d)
         (cdro l d)
         (lolo d))))))

(test-check "testc14.tex-9"
  (run 1 (l)
    (lolo l))
  `(()))

(test-check "testc14.tex-10"
  (run* (q)
    (fresh (x y)
      (lolo `((a b) (,x c) (d ,y)))
      (== #t q)))
  (list #t))

(test-check "testc14.tex-11"
  (run 1 (q)
    (fresh (x)
      (lolo `((a b) . ,x))
      (== #t q)))
  (list #t))

(test-check "testc14.tex-12"
  (run 1 (x)
    (lolo `((a b) (c d) . ,x)))
  `(()))

(test-check "testc14.tex-13"
  (run 5 (x)
    (lolo `((a b) (c d) . ,x)))
  `(()
    (())
    ((_.0))
    (() ())
    ((_.0 _.1))))

(define twinso
  (lambda (s)
    (fresh (x y)
      (conso x y s)
      (conso x '() y))))

(test-check "testc14.tex-14"
  (run* (q)
    (twinso '(tofu tofu))
    (== #t q))
  (list #t))

(test-check "testc14.tex-15"
  (run* (z)
    (twinso `(,z tofu)))
  (list `tofu))

(define loto
  (lambda (l)
    (conde
      ((nullo l) succeed)
      ((fresh (a)
         (caro l a)
         (twinso a))
       (fresh (d)
         (cdro l d)
         (loto d))))))

(test-check "testc14.tex-16"
  (run 1 (z)
    (loto `((g g) . ,z)))
  (list `()))

(test-check "testc14.tex-17"
  (run 5 (z)
    (loto `((g g) . ,z)))
  '(()
    ((_.0 _.0))
    ((_.0 _.0) (_.1 _.1))
    ((_.0 _.0) (_.1 _.1) (_.2 _.2))
    ((_.0 _.0) (_.1 _.1) (_.2 _.2) (_.3 _.3))))

(test-check "testc14.tex-18"
  (run 5 (r)
    (fresh (w x y z)
      (loto `((g g) (e ,w) (,x ,y) . ,z))
      (== `(,w (,x ,y) ,z) r)))
  '((e (_.0 _.0) ())
    (e (_.0 _.0) ((_.1 _.1)))
    (e (_.0 _.0) ((_.1 _.1) (_.2 _.2)))
    (e (_.0 _.0) ((_.1 _.1) (_.2 _.2) (_.3 _.3)))
    (e (_.0 _.0) ((_.1 _.1) (_.2 _.2) (_.3 _.3) (_.4 _.4)))))

(test-check "testc14.tex-19"
  (run 3 (out)
    (fresh (w x y z)
      (== `((g g) (e ,w) (,x ,y) . ,z) out)
      (loto out)))
  `(((g g) (e e) (_.0 _.0))
    ((g g) (e e) (_.0 _.0) (_.1 _.1))
    ((g g) (e e) (_.0 _.0) (_.1 _.1) (_.2 _.2))))

(define listofo
  (lambda (predo l)
    (conde
      ((nullo l) succeed)
      ((fresh (a)
         (caro l a)
         (predo a))
       (fresh (d)
         (cdro l d)
         (listofo predo d))))))

(test-check "testc14.tex-20"
  (run 3 (out)
    (fresh (w x y z)
      (== `((g g) (e ,w) (,x ,y) . ,z) out)
      (listofo twinso out)))
  `(((g g) (e e) (_.0 _.0))
    ((g g) (e e) (_.0 _.0) (_.1 _.1))
    ((g g) (e e) (_.0 _.0) (_.1 _.1) (_.2 _.2))))

(define loto
  (lambda (l)
    (listofo twinso l)))

(define member?
  (lambda (x l)
    (cond
      ((null? l) #f)
      ((eq? (car l) x) #t)
      (else (member? x (cdr l))))))

(test-check "testc14.tex-21"
  (member? 'olive `(virgin olive oil))
  #t)

(define membero
  (lambda (x l)
    (conde
      ((nullo l) fail)
      ((fresh (a)
         (caro l a)
         (== a x))
       succeed)
      (succeed
        (fresh (d)
          (cdro l d)
          (membero x d))))))

(test-check "testc14.tex-22"
  (run* (q)
    (membero 'olive `(virgin olive oil))
    (== #t q))
  (list #t))

(test-check "testc14.tex-23"
  (run 1 (y)
    (membero y `(hummus with pita)))
  (list `hummus))

(test-check "testc14.tex-24"
  (run 1 (y)
    (membero y `(with pita)))
  (list `with))

(test-check "testc14.tex-25"
  (run 1 (y)
    (membero y `(pita)))
  (list `pita))

(test-check "testc14.tex-26"
  (run* (y)
    (membero y `()))
  `())

(test-check "testc14.tex-27"
  (run* (y)
    (membero y `(hummus with pita)))
  `(hummus with pita))

(test-check "testc14.tex-28"
  (run* (x)
    (membero 'e `(pasta ,x fagioli)))
  (list `e))

(test-check "testc14.tex-29"
  (run 1 (x)
    (membero 'e `(pasta e ,x fagioli)))
  (list `_.0))

(test-check "testc14.tex-30"
  (run 1 (x)
    (membero 'e `(pasta ,x e fagioli)))
  (list `e))

(test-check "testc14.tex-31"
  (run* (r)
    (fresh (x y)
      (membero 'e `(pasta ,x fagioli ,y))
      (== `(,x ,y) r)))
  `((e _.0) (_.0 e)))

(test-check "testc14.tex-32"
  (run 1 (l)
    (membero 'tofu l))
  `((tofu . _.0)))

(define e
  (make-engine
    (lambda ()
      (run* (l)
        (membero 'tofu l)))))

(printf "Testing testc14.tex-33  (engine with ~s ticks fuel)\n" max-ticks)
(e max-ticks
   (lambda (t v) (error 'testc14.tex-33 "infinite loop returned ~s after ~s ticks" v (- max-ticks t)))
   (lambda (e^) (void)))

(test-check "testc14.tex-34"
  (run 5 (l)
    (membero 'tofu l))
  `((tofu . _.0)
    (_.0 tofu . _.1)
    (_.0 _.1 tofu . _.2)
    (_.0 _.1 _.2 tofu . _.3)
    (_.0 _.1 _.2 _.3 tofu . _.4)))

(define pmembero
  (lambda (x l)
    (conde
      ((caro l x) (cdro l '()))
      ((fresh (d)
         (cdro l d)
         (pmembero x d))))))

(test-check "testc14.tex-35"
  (run 5 (l)
    (pmembero 'tofu l))
  `((tofu)
    (_.0 tofu)
    (_.0 _.1 tofu)
    (_.0 _.1 _.2 tofu)
    (_.0 _.1 _.2 _.3 tofu)))

(test-check "testc14.tex-36"
  (run* (q)
    (pmembero 'tofu `(a b tofu d tofu))
    (== #t q))
  `(#t))

(define pmembero
  (lambda (x l)
    (conde
      ((caro l x)
       (conde
         ((cdro l '()))
         (succeed)))
      ((fresh (d)
         (cdro l d)
         (pmembero x d))))))

(test-check "testc14.tex-37"
  (run* (q)
    (pmembero 'tofu `(a b tofu d tofu))
    (== #t q))
  `(#t #t #t))

(define pmembero
  (lambda (x l)
    (conde
      ((caro l x)
       (conde
         ((cdro l '()))
         ((fresh (a d)
            (cdro l `(,a . ,d))))))
      ((fresh (d)
         (cdro l d)
         (pmembero x d))))))

(test-check "testc14.tex-38"
  (run* (q)
    (pmembero 'tofu `(a b tofu d tofu))
    (== #t q))
  `(#t #t))

(test-check "testc14.tex-39"
  (run 12 (l)
    (pmembero 'tofu l))
  `((tofu)
    (tofu _.0 . _.1)
    (_.0 tofu)
    (_.0 tofu _.1 . _.2)
    (_.0 _.1 tofu)
    (_.0 _.1 tofu _.2 . _.3)
    (_.0 _.1 _.2 tofu)
    (_.0 _.1 _.2 tofu _.3 . _.4)
    (_.0 _.1 _.2 _.3 tofu)
    (_.0 _.1 _.2  _.3 tofu _.4 . _.5 )
    (_.0 _.1 _.2 _.3 _.4 tofu)
    (_.0 _.1 _.2 _.3 _.4 tofu _.5 . _.6)))

(define mem
  (lambda (x l)
    (cond
      ((null? l) #f)
      ((eq? (car l) x) l)
      (else (mem x (cdr l))))))

(test-equal "testc15.tex-1"
  (mem 'tofu `(a b tofu d peas e))
  `(tofu d peas e))

(test-equal "testc15.tex-2"
  (mem 'tofu `(a b peas d peas e))
  #f)

(test-check "testc15.tex-3"
  (run* (out)
    (== (mem 'tofu `(a b tofu d peas e)) out))
  (list `(tofu d peas e)))

(test-equal "testc15.tex-4"
  (mem 'peas (mem 'tofu `(a b tofu d peas e)))
  `(peas e))

(test-equal "testc15.tex-5"
  (mem 'tofu (mem 'tofu `(a b tofu d tofu e)))
  `(tofu d tofu e))

(test-equal "testc15.tex-6"
  (mem 'tofu (cdr (mem 'tofu `(a b tofu d tofu e))))
  `(tofu e))

(define memo
  (lambda (x l out)
    (conde
      ((nullo l) fail)
      ((fresh (a)
         (caro l a)
         (== a x))
       (== l out))
      (succeed
        (fresh (d)
          (cdro l d)
          (memo x d out))))))

(define memo
  (lambda (x l out)
    (conde
      ((fresh (a)
         (caro l a)
         (== a x))
       (== l out))
      ((fresh (d)
         (cdro l d)
         (memo x d out))))))

(define memo
  (lambda (x l out)
    (conde
      ((caro l x) (== l out))
      ((fresh (d)
         (cdro l d)
         (memo x d out))))))

(test-check "testc15.tex-7"
  (run 1 (out)
    (memo 'tofu `(a b tofu d tofu e) out))
  `((tofu d tofu e)))

(test-check "testc15.tex-8"
  (run 1 (out)
    (fresh (x)
      (memo 'tofu `(a b ,x d tofu e) out)))
  `((tofu d tofu e)))

(test-check "testc15.tex-9"
  (run* (r)
    (memo
      r
      `(a b tofu d tofu e)
      `(tofu d tofu e)))
  (list `tofu))

(test-check "testc15.tex-10"
  (run* (q)
    (memo 'tofu '(tofu e) '(tofu e))
    (== #t q))
  (list #t))

(test-check "testc15.tex-11"
  (run* (q)
    (memo 'tofu '(tofu e) '(tofu))
    (== #t q))
  `())

(test-check "testc15.tex-12"
  (run* (x)
    (memo 'tofu '(tofu e) `(,x e)))
  (list `tofu))

(test-check "testc15.tex-13"
  (run* (x)
    (memo 'tofu '(tofu e) `(peas ,x)))
  `())

(test-check "testc15.tex-14"
  (run* (out)
    (fresh (x)
      (memo 'tofu `(a b ,x d tofu e) out)))
  `((tofu d tofu e) (tofu e)))

(test-check "testc15.tex-15"
  (run 12 (z)
    (fresh (u)
      (memo 'tofu `(a b tofu d tofu e . ,z) u)))
  `(_.0
     _.0
     (tofu . _.0)
     (_.0 tofu . _.1)
     (_.0 _.1 tofu . _.2)
     (_.0 _.1 _.2 tofu . _.3)
     (_.0 _.1 _.2 _.3 tofu . _.4)
     (_.0 _.1 _.2 _.3 _.4 tofu . _.5)
     (_.0 _.1 _.2 _.3 _.4 _.5 tofu . _.6)
     (_.0 _.1 _.2 _.3 _.4 _.5 _.6 tofu . _.7)
     (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 tofu . _.8)
     (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 _.8 tofu . _.9)))

(define rember
  (lambda (x l)
    (cond
      ((null? l) '())
      ((eq? (car l) x) (cdr l))
      (else
        (cons (car l)
              (rember x (cdr l)))))))

(test-equal "testc15.tex-16"
  (rember 'peas '(a b peas d peas e))
  `(a b d peas e))

(define rembero
  (lambda (x l out)
    (conde
      ((nullo l) (== '() out))
      ((fresh (a)
         (caro l a)
         (== a x))
       (cdro l out))
      ((fresh (res)
         (fresh (d)
           (cdro l d)
           (rembero x d res))
         (fresh (a)
           (caro l a)
           (conso a res out)))))))

(define rembero
  (lambda (x l out)
    (conde
      ((nullo l) (== '() out))
      ((caro l x) (cdro l out))
      ((fresh (res)
         (fresh (d)
           (cdro l d)
           (rembero x d res))
         (fresh (a)
           (caro l a)
           (conso a res out)))))))

(define rembero
  (lambda (x l out)
    (conde
      ((nullo l) (== '() out))
      ((caro l x) (cdro l out))
      ((fresh (a d res)
         (conso a d l)
         (rembero x d res)
         (conso a res out))))))

(test-check "testc15.tex-17"
  (run 1 (out)
    (fresh (y)
      (rembero 'peas `(a b ,y d peas e) out)))
  `((a b d peas e)))

(test-check "testc15.tex-18"
  (run* (out)
    (fresh (y z)
      (rembero y `(a b ,y d ,z e) out)))
  `((b a d _.0 e)
    (a b d _.0 e)
    (a b d _.0 e)
    (a b d _.0 e)
    (a b _.0 d e)
    (a b e d _.0)
    (a b _.0 d _.1 e)))

(test-check "testc15.tex-19"
  (run* (r)
    (fresh (y z)
      (rembero y `(,y d ,z e) `(,y d e))
      (== `(,y ,z) r)))
  `((d d)
    (d d)
    (_.0 _.0)
    (e e)))

(test-check "testc15.tex-20"
  (run 13 (w)
    (fresh (y z out)
      (rembero y `(a b ,y d ,z . ,w) out)))
  `(_.0
     _.0
     _.0
     _.0
     _.0
     ()
     (_.0 . _.1)
     (_.0)
     (_.0 _.1 . _.2)
     (_.0 _.1)
     (_.0 _.1 _.2 . _.3)
     (_.0 _.1 _.2)
     (_.0 _.1 _.2 _.3 . _.4)))

(define surpriseo
  (lambda (s)
    (rembero s '(a b c) '(a b c))))

(test-check "testc15.tex-21"
  (run* (r)
    (== 'd r)
    (surpriseo r))
  (list 'd))

(test-check "testc15.tex-22"
  (run* (r)
    (surpriseo r))
  `(_.0))

(test-check "testc15.tex-23"
  (run* (r)
    (== 'b r)
    (surpriseo r))
  `(b))

(define new-append
  (lambda (l s)
    (cond
      ((null? l) s)
      (else (cons (car l)
                  (new-append (cdr l) s))))))

(test-equal "testc16.tex-1"
  (new-append `(a b c) `(d e))
  `(a b c d e))

(test-equal "testc16.tex-2"
  (new-append '(a b c) '())
  `(a b c))

(test-equal "testc16.tex-3"
  (new-append '() '(d e))
  `(d e))

(test-equal "testc16.tex-4"
  (new-append '(d e) 'a)
  `(d e . a))

(define appendo
  (lambda (l s out)
    (conde
      ((nullo l) (== s out))
      ((fresh (a d res)
         (caro l a)
         (cdro l d)
         (appendo d s res)
         (conso a res out))))))

(test-check "testc16.tex-5"
  (run* (x)
    (appendo
      '(cake)
      '(tastes yummy)
      x))
  (list `(cake tastes yummy)))

(test-check "testc16.tex-6"
  (run* (x)
    (fresh (y)
      (appendo
        `(cake with ice ,y)
        '(tastes yummy)
        x)))
  (list `(cake with ice _.0 tastes yummy)))

(test-check "testc16.tex-7"
  (run* (x)
    (fresh (y)
      (appendo
        '(cake with ice cream)
        y
        x)))
  (list `(cake with ice cream . _.0)))

(test-check "testc16.tex-8"
  (run 1 (x)
    (fresh (y)
      (appendo `(cake with ice . ,y) '(d t) x)))
  (list `(cake with ice d t)))

(test-check "testc16.tex-9"
  (run 1 (y)
    (fresh (x)
      (appendo `(cake with ice . ,y) '(d t) x)))
  (list '()))

(define appendo
  (lambda (l s out)
    (conde
      ((nullo l) (== s out))
      ((fresh (a d res)
         (conso a d l)
         (appendo d s res)
         (conso a res out))))))

(test-check "testc16.tex-10"
  (run 5 (x)
    (fresh (y)
      (appendo `(cake with ice . ,y) '(d t) x)))
  `((cake with ice d t)
    (cake with ice _.0 d t)
    (cake with ice _.0 _.1 d t)
    (cake with ice _.0 _.1 _.2 d t)
    (cake with ice _.0 _.1 _.2 _.3 d t)))

(test-check "testc16.tex-11"
  (run 5 (y)
    (fresh (x)
      (appendo `(cake with ice . ,y) '(d t) x)))
  `(()
    (_.0)
    (_.0 _.1)
    (_.0 _.1 _.2)
    (_.0 _.1 _.2 _.3)))

(define y `(_.0 _.1 _.2))

(test-check "testc16.tex-12"
  `(cake with ice . ,y)
  `(cake with ice . (_.0 _.1 _.2)))

(test-check "testc16.tex-13"
  (run 5 (x)
    (fresh (y)
      (appendo
        `(cake with ice . ,y)
        `(d t . ,y)
        x)))
  `((cake with ice d t)
    (cake with ice _.0 d t _.0)
    (cake with ice _.0 _.1 d t _.0 _.1)
    (cake with ice _.0 _.1 _.2 d t _.0 _.1 _.2)
    (cake with ice _.0 _.1 _.2 _.3 d t _.0 _.1 _.2 _.3)))

(test-check "testc16.tex-14"
  (run* (x)
    (fresh (z)
      (appendo
        `(cake with ice cream)
        `(d t . ,z)
        x)))
  `((cake with ice cream d t . _.0)))

(test-check "testc16.tex-15"
  (run 6 (x)
    (fresh (y)
      (appendo x y `(cake with ice d t))))
  `(()
    (cake)
    (cake with)
    (cake with ice)
    (cake with ice d)
    (cake with ice d t)))

(test-check "testc16.tex-16"
  (run 6 (y)
    (fresh (x)
      (appendo x y `(cake with ice d t))))
  `((cake with ice d t)
    (with ice d t)
    (ice d t)
    (d t)
    (t)
    ()))

(define appendxyquestion
  (lambda ()
    (run 6 (r)
      (fresh (x y)
        (appendo x y `(cake with ice d t))
        (== `(,x ,y) r)))))

(define appendxyanswer
  `((() (cake with ice d t))
    ((cake) (with ice d t))
    ((cake with) (ice d t))
    ((cake with ice) (d t))
    ((cake with ice d) (t))
    ((cake with ice d t) ())))

(test-check "appendxy"
  (appendxyquestion)
  appendxyanswer)

(define e
  (make-engine
    (lambda ()
      (run 7 (r)
        (fresh (x y)
          (appendo x y `(cake with ice d t))
          (== `(,x ,y) r))))))

(printf "Testing testc16.tex-17  (engine with ~s ticks fuel)\n" max-ticks)
(e max-ticks
   (lambda (t v) (error 'testc16.tex-17 "infinite loop returned ~s after ~s ticks" v (- max-ticks t)))
   (lambda (e^) (void)))

(define appendo
  (lambda (l s out)
    (conde
      ((nullo l) (== s out))
      ((fresh (a d res)
         (conso a d l)
         (conso a res out)
         (appendo d s res))))))

(test-check "testc16.tex-18"
  (run 7 (r)
    (fresh (x y)
      (appendo x y `(cake with ice d t))
      (== `(,x ,y) r)))
  appendxyanswer)

(test-check "testc16.tex-19"
  (run 7 (x)
    (fresh (y z)
      (appendo x y z)))
  `(()
    (_.0)
    (_.0 _.1)
    (_.0 _.1 _.2)
    (_.0 _.1 _.2 _.3)
    (_.0 _.1 _.2 _.3 _.4)
    (_.0 _.1 _.2 _.3 _.4 _.5)))

(test-check "testc16.tex-20"
  (run 7 (y)
    (fresh (x z)
      (appendo x y z)))
  `(_.0
     _.0
     _.0
     _.0
     _.0
     _.0
     _.0))

(test-check "testc16.tex-21"
  (run 7 (z)
    (fresh (x y)
      (appendo x y z)))
  `(_.0
     (_.0 . _.1)
     (_.0 _.1 . _.2)
     (_.0 _.1 _.2 . _.3)
     (_.0 _.1 _.2 _.3 . _.4)
     (_.0 _.1 _.2 _.3 _.4 . _.5)
     (_.0 _.1 _.2 _.3 _.4 _.5 . _.6)))

(test-check "testc16.tex-22"
  (run 7 (r)
    (fresh (x y z)
      (appendo x y z)
      (== `(,x ,y ,z) r)))
  `((() _.0 _.0)
    ((_.0) _.1 (_.0 . _.1))
    ((_.0 _.1) _.2 (_.0 _.1 . _.2))
    ((_.0 _.1 _.2) _.3 (_.0 _.1 _.2 . _.3))
    ((_.0 _.1 _.2 _.3) _.4 (_.0 _.1 _.2 _.3 . _.4))
    ((_.0 _.1 _.2 _.3 _.4) _.5 (_.0 _.1 _.2 _.3 _.4 . _.5))
    ((_.0 _.1 _.2 _.3 _.4 _.5) _.6 (_.0 _.1 _.2 _.3 _.4 _.5 . _.6))))

(define swappendo
  (lambda (l s out)
    (conde
      ((fresh (a d res)
         (conso a d l)
         (conso a res out)
         (swappendo d s res)))
      ((nullo l) (== s out)))))

(test-check "testc16.tex-23"
  (run 7 (r)
    (fresh (x y z)
      (swappendo x y z)
      (== `(,x ,y ,z) r)))
  `((() _.0 _.0)
    ((_.0) _.1 (_.0 . _.1))
    ((_.0 _.1) _.2 (_.0 _.1 . _.2))
    ((_.0 _.1 _.2) _.3 (_.0 _.1 _.2 . _.3))
    ((_.0 _.1 _.2 _.3) _.4 (_.0 _.1 _.2 _.3 . _.4))
    ((_.0 _.1 _.2 _.3 _.4) _.5 (_.0 _.1 _.2 _.3 _.4 . _.5))
    ((_.0 _.1 _.2 _.3 _.4 _.5) _.6 (_.0 _.1 _.2 _.3 _.4 _.5 . _.6))))

(define unwrap
  (lambda (x)
    (cond
      ((pair? x) (unwrap (car x)))
      (else x))))

(test-equal "testc16.tex-24"
  (unwrap '((((pizza)))))
  `pizza)

(test-equal "testc16.tex-25"
  (unwrap '((((pizza pie) with)) extra cheese))
  `pizza)

(define unwrapo
  (lambda (x out)
    (conde
      ((pairo x)
       (fresh (a)
         (caro x a)
         (unwrapo a out)))
      ((== x out)))))

(test-check "testc16.tex-26"
  (run* (x)
    (unwrapo '(((pizza))) x))
  `((((pizza)))
    ((pizza))
    (pizza)
    pizza))

(test-check "testc16.tex-27"
  (run 1 (x)
    (unwrapo x 'pizza))
  `(pizza))

(test-check "testc16.tex-28"
  (run 1 (x)
    (unwrapo `((,x)) 'pizza))
  `(pizza))

(test-check "testc16.tex-29"
  (run 5 (x)
    (unwrapo x 'pizza))
  `(pizza
     (pizza . _.0)
     ((pizza . _.0) . _.1)
     (((pizza . _.0) . _.1) . _.2)
     ((((pizza . _.0) . _.1) . _.2) . _.3)))

(test-check "testc16.tex-30"
  (run 5 (x)
    (unwrapo x '((pizza))))
  `(((pizza))
    (((pizza)) . _.0)
    ((((pizza)) . _.0) . _.1)
    (((((pizza)) . _.0) . _.1) . _.2)
    ((((((pizza)) . _.0) . _.1) . _.2) . _.3)))

(test-check "testc16.tex-31"
  (run 5 (x)
    (unwrapo `((,x)) 'pizza))
  `(pizza
     (pizza . _.0)
     ((pizza . _.0) . _.1)
     (((pizza . _.0) . _.1) . _.2)
     ((((pizza . _.0) . _.1) . _.2) . _.3)))

(define flatten
  (lambda (s)
    (cond
      ((null? s) '())
      ((pair? s)
       (new-append
         (flatten (car s))
         (flatten (cdr s))))
      (else (cons s '())))))

(test-equal "testc16.tex-32"
  (flatten '((a b) c))
  `(a b c))

(define flatteno
  (lambda (s out)
    (conde
      ((nullo s) (== '() out))
      ((pairo s)
       (fresh (a d res-a res-d)
         (conso a d s)
         (flatteno a res-a)
         (flatteno d res-d)
         (appendo res-a res-d out)))
      ((conso s '() out)))))

(test-check "testc16.tex-33"
  (run 10 (x)
    (flatteno '((a b) c) x))
  `((((a b) c))
    ((a b) (c))
    ((a b) c)
    ((a b) c ())
    (a (b) (c))
    (a (b) c)
    (a (b) c ())
    (a b (c))
    (a b () (c))
    (a b c)))

(test-check "testc16.tex-34"
  (run 10 (x)
    (flatteno '(a (b c)) x))
  `(((a (b c)))
    (a ((b c)))
    (a (b c))
    (a (b c) ())
    (a b (c))
    (a b (c) ())
    (a b c)
    (a b c ())
    (a b c ())
    (a b c () ())))

(test-check "testc16.tex-35"
  (run* (x)
    (flatteno '(a) x))
  `(((a))
    (a)
    (a ())))

(test-check "testc16.tex-36"
  (run* (x)
    (flatteno '((a)) x))
  `((((a)))
    ((a))
    ((a) ())
    (a)
    (a ())
    (a ())
    (a () ())))

(test-check "testc16.tex-37"
  (run* (x)
    (flatteno '(((a))) x))
  `(((((a))))
    (((a)))
    (((a)) ())
    ((a))
    ((a) ())
    ((a) ())
    ((a) () ())
    (a)
    (a ())
    (a ())
    (a () ())
    (a ())
    (a () ())
    (a () ())
    (a () () ())))

(define flattenogrumblequestion
  (lambda ()
    (run* (x)
      (flatteno '((a b) c) x))))

(define flattenogrumbleanswer
  `((((a b) c))
    ((a b) (c))
    ((a b) c)
    ((a b) c ())
    (a (b) (c))
    (a (b) c)
    (a (b) c ())
    (a b (c))
    (a b () (c))
    (a b c)
    (a b c ())
    (a b () c)
    (a b () c ())))

(test-check "flattenogrumble"
  (flattenogrumblequestion)
  flattenogrumbleanswer)

(define e
  (make-engine
    (lambda ()
      (run* (x)
        (flatteno x '(a b c))))))

(printf "Testing testc16.tex-38  (engine with ~s ticks fuel)\n" max-ticks)
(e max-ticks
   (lambda (t v) (error 'testc16.tex-38 "infinite loop returned ~s after ~s ticks" v (- max-ticks t)))
   (lambda (e^) (void)))

(test-check "testc16.tex-39"
  (length
    (run* (x)
      (flatteno '((((a (((b))) c))) d) x)))
  574)
