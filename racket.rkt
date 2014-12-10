#lang racket

(define (step ς)
  (match ς
    [`(,e ,ρ ,κ)
     (match e
       [(? symbol?)     (apply-κ κ (hash-ref ρ e))]
       [(? number?)     (apply-κ κ e)]
       [`(cons ,e1 ,e2) `(,e1 ,ρ (cons1 ,e2 ,ρ ,κ))]
       [`(λ (,x) ,b)    (apply-κ κ `(closure (λ (,x) ,b) ,ρ))]
       [`(,a ,b)        `(,a ,ρ (app1 ,b ,ρ ,κ))])]))

(define (apply-κ κ e)
  (match κ
    ['halt            `(done ,e)]
    [`(app1 ,b ,ρ ,κ) `(,b ,ρ (app2 ,e ,κ))]
    [`(app2 (closure (λ (,x) ,b) ,ρ) ,κ)
     `(,b ,(hash-set ρ x e) ,κ)]
    [`(cons1 ,e2 ,ρ ,κ)
     `(,e2 ,ρ (cons2 ,e ,κ))]
    [`(cons2 ,v1 ,κ)
     (apply-κ κ `(pair ,v1 ,e))]))

(define (step* ς)
  (let ([new-state (step ς)])
  (match new-state
    [`(done ,result) result]
    [else (step* new-state)])))

