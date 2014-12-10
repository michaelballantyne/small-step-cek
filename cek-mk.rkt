#lang racket

(require "mk.rkt")

(define lookupo
  (lambda (x env t)
    (fresh (rest y v)
      (== `((,y . ,v) . ,rest) env)
      (conde
        ((== y x) (== v t))
        ((=/= y x) (lookupo x rest t))))))

(define (stepo state state^)
  (fresh (e env k)
    (== `(,e ,env ,k) state)
    (conde
      [(symbolo e)
       (fresh (v)
         (lookupo e env v)
         (apply-ko k v state^))]
      [(numbero e)
       (apply-ko k e state^)]
      [(fresh (e1 e2)
         (== `(cons ,e1 ,e2) e)
         (== `(,e1 ,env (cons1 ,e2 ,env ,k)) state^))]
      [(fresh (x b)
         (== `(lambda (,x) ,b) e)
         (apply-ko k `(closure (lambda (,x) ,b) ,env) state^))]
      [(fresh (a b)
         (== `(,a ,b) e)
         (== `(,a ,env (app1 ,b ,env ,k)) state^))])))

(define (apply-ko k v state^)
  (conde
    [(== 'halt k)
     (== `(done ,v) state^)]
    [(fresh (e2 env k^)
       (== `(cons1 ,e2 ,env ,k^) k)
       (== `(,e2 ,env (cons2 ,v ,k^)) state^))]
    [(fresh (v1 k^)
       (== `(cons2 ,v1 ,k^) k)
       (apply-ko k^ `(pair ,v1 ,v) state^))]
    [(fresh (b env k^)
       (== `(app1 ,b ,env ,k^) k)
       (== `(,b ,env (app2 ,v ,k^)) state^))]
    [(fresh (x b env k^)
       (== `(app2 (closure (lambda (,x) ,b) ,env) ,k^) k)
       (== `(,b ((,x . ,v) . ,env) ,k^) state^))]))

(define (step*o state out)
  (fresh (state^)
    (stepo state state^)
    (conde
      [(== `(done ,out) state^)]
      [(step*o state^ out)])))

(define (inject e)
  `(,e () halt))

(module+ test
  (require rackunit)

  (check-equal?
    (run* (q) (lookupo 'a '((a . 5)) q))
    '(5))

  (check-equal?
    (run* (q) (step*o `(a ((a . 5)) halt) q))
    '(5))

  (check-equal?
    (run* (q) (step*o (inject '5) q))
    '(5))

  (check-equal? (run 2 (q) (step*o (inject '(cons 1 2)) q)) '((pair 1 2)))

  (check-equal? (run* (q) (step*o (inject '(cons (cons 1 2) (cons 2 3))) q))
                '((pair (pair 1 2) (pair 2 3))))

  (check-equal?
    (run* (q) (step*o (inject '((lambda (x) x) 5)) q))
    '(5))

  (check-equal?
    (run* (q) (step*o (inject '(((lambda (x) (lambda (y) x)) 4) 5)) q))
    '(4))

  (check-equal?
    (run 1 (q) (fresh (x y) (step*o (inject `(cons 5 ,x)) `(pair 6 ,y))))
    '())
  )
