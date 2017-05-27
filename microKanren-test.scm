(load "miniKanren-wrappers.scm")
(load "microKanren-test-programs.scm")

(test-check "second-set t1"
  (let (($ ((call/fresh (lambda (q) (== q 5))) empty-state)))
    (reify-1st (car $)))
  5)

(test-check "second-set t2"
  (let (($ ((call/fresh (lambda (q) (== q 5))) empty-state)))
    (cdr $))
  '())

(test-check "second-set t3"
  (let (($ (a-and-b empty-state)))
    (reify-1st (car $)))
  7)

(test-check "second-set t3, take"
  (let (($ (a-and-b empty-state)))
    (map reify-1st (take 1 $)))
  '(7))

(test-check "second-set t4"
  (let (($ (a-and-b empty-state)))
    (reify-1st (car (cdr $))))
  7)

(test-check "second-set t5"
  (let (($ (a-and-b empty-state)))
    (cdr (cdr $)))
  '())

(test-check "who cares"
  (let (($ ((call/fresh (lambda (q) (fives q))) empty-state)))
    (map reify-1st (take 1 $)))
  '(5))

(test-check "take 2 a-and-b stream"
  (let (($ (a-and-b empty-state)))
    (map reify-1st (take 2 $)))
  '(7 7))

(test-check "take-all a-and-b stream"
  (let (($ (a-and-b empty-state)))
    (map reify-1st (take-all $)))
  '(7 7))

(test-check "ground appendo"
  (reify-1st (car ((ground-appendo empty-state))))
  'a)

(test-check "ground appendo2"
  (reify-1st (car ((ground-appendo2 empty-state))))
  'a)

(test-check "appendo"
  (map reify-1st (take 2 (call-appendo empty-state)))
  '((() _.0 _.0) ((_.0) _.1 (_.0 . _.1))))

(test-check "appendo2"
  (map reify-1st (take 2 (call-appendo2 empty-state)))
  '((() _.0 _.0) ((_.0) _.1 (_.0 . _.1))))

(test-check "many non-ans"
  (map reify-1st (take 1 (many-non-ans empty-state)))
  '(3))

(test-check "run*"
  (run* (q) (fresh (x y) (== `(,x ,y) q) (appendo x y '(1 2 3 4 5))))
  '((() (1 2 3 4 5))
    ((1) (2 3 4 5))
    ((1 2) (3 4 5))
    ((1 2 3) (4 5))
    ((1 2 3 4) (5))
    ((1 2 3 4 5) ())))

(test-check "run*2"
  (run* (q x y) (== `(,x ,y) q) (appendo x y '(1 2 3 4 5)))
  '((() (1 2 3 4 5))
    ((1) (2 3 4 5))
    ((1 2) (3 4 5))
    ((1 2 3) (4 5))
    ((1 2 3 4) (5))
    ((1 2 3 4 5) ())))

(test-check "rember*o"
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

(test-check "rember*o"
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

(load "more-tests.scm")
