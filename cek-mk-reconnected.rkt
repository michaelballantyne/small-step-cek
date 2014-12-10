#lang racket

(require "mk.rkt")

(define lookupo
  (lambda (x env t)
    (fresh (rest y v)
      (== `((,y . ,v) . ,rest) env)
      (conde
        ((== y x) (== v t))
        ((=/= y x) (lookupo x rest t))))))

(define (apply-ko k state^)
  (== k state^))

(define (stepo state state^)
  (fresh (e env k out)
    (== `(,e ,env ,out ,k) state)
    (conde
      [(symbolo e)
       (fresh (v)
         (== v out)
         (apply-ko k state^)
         (lookupo e env v))]
      [(numbero e)
       (== e out)
       (apply-ko k state^)]
      [(fresh (e1 e2 v1 v2)
         (== `(cons ,e1 ,e2) e)
         (== `(pair ,v1 ,v2) out)
         (== `(,e1 ,env ,v1 (,e2 ,env ,v2 ,k)) state^))]
      [(fresh (x b)
         (== `(lambda (,x) ,b) e)
         (== `(closure (lambda (,x) ,b) ,env) out)
         (apply-ko k state^))]
      [(fresh (a b av bv x body env^)
         (== `(,a ,b) e)
         (== `(closure (lambda (,x) ,body) ,env^) av)
         (== `(,a ,env ,av
                  (,b ,env ,bv
                      (,body ((,x . ,bv) . ,env^) ,out ,k)))
             state^))])))

(define (step*o state)
  (fresh (state^)
    (stepo state state^)
    (conde
      [(== 'halt state^)]
      [(step*o state^)])))

(define (step-driver exp env out)
  (step*o `(,exp ,env ,out halt)))

(module+ test
  (require "test-check.rkt")

  (test "lookupo"
    (run* (q) (lookupo 'a '((a . 5)) q))
    '(5))

  (test "variable lookup"
    (run* (q) (step-driver 'a '((a . 5)) q))
    '(5))

  (test "numeric literals"
    (run* (q) (step-driver '5 '() q))
    '(5))

  (test "cons"
    (run 2 (q) (step-driver '(cons 1 2) '()  q))
    '((pair 1 2)))

  (test "nested cons"
    (run* (q) (step-driver '(cons (cons 1 2) (cons 2 3)) '() q))
    '((pair (pair 1 2) (pair 2 3))))

  (test "identity function"
    (run* (q) (step-driver '((lambda (x) x) 5) '() q))
    '(5))

  (test "true function"
    (run* (q) (step-driver '(((lambda (x) (lambda (y) x)) 4) 5) '() q))
    '(4))

  (test "cons refutaton in car position"
    (run 1 (q) (fresh (x y) (step-driver `(cons 5 ,x) '() `(pair 6 ,y))))
    '())

  (displayln "\nshould diverge:")
  ; Diverges, as expected.
  (test "cons refutation in cdr position"
    (run 1 (q) (fresh (x y) (step-driver `(cons ,x 5) '() `(pair ,y 6))))
    '())
  )
