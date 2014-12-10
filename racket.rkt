#lang racket

(define (step ς)
  (match ς
    [`(,e ,ρ ,κ)
     (match e
       [(? symbol?)     (apply-κ κ (hash-ref ρ e))]
       [(? number?)     (apply-κ κ e)]
       [`(cons ,e1 ,e2) `(,e1 ,ρ (cons1 ,e2 ,ρ ,κ))]
       [`(lambda (,x) ,b)    (apply-κ κ `(closure (lambda (,x) ,b) ,ρ))]
       [`(,a ,b)        `(,a ,ρ (app1 ,b ,ρ ,κ))])]))

(define (apply-κ κ v)
  (match κ
    ['halt            `(done ,v)]
    [`(app1 ,b ,ρ ,κ) `(,b ,ρ (app2 ,v ,κ))]
    [`(app2 (closure (lambda (,x) ,b) ,ρ) ,κ)
     `(,b ,(hash-set ρ x v) ,κ)]
    [`(cons1 ,e2 ,ρ ,κ)
     `(,e2 ,ρ (cons2 ,v ,κ))]
    [`(cons2 ,v1 ,κ)
     (apply-κ κ `(pair ,v1 ,v))]))

(define (step* ς)
  (let ([new-state (step ς)])
  (match new-state
    [`(done ,result) result]
    [else (step* new-state)])))


(module+ test
  (require rackunit)

  (check-equal? (step* `(((lambda (x) x) 5) ,(hash) halt))
                5)

  (check-equal? (step* `((cons (cons ((lambda (x) x) 1) 2) (cons 2 3)) ,(hash) halt))
                '(pair (pair 1 2) (pair 2 3))))
