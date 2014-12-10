#lang racket

(require rackunit/log)

(provide test)

(define-syntax test
  (syntax-rules ()
    ((_ title tested-expression expected-result)
     (begin
       (printf "Testing ~a~n" title)
       (let* ((expected expected-result)
              (produced tested-expression)
              (result (equal? expected produced)))
         (test-log! result)
         (when (not result)
           (printf "Failed: ")
           (pretty-print 'tested-expression)
           (printf "Expected: ")
           (pretty-print expected)
           (printf "Computed: ")
           (pretty-print produced)
           (printf "~n")
           ))))))
