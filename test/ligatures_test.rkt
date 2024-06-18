#lang eopl
(require rackunit)
(require "../main.rkt")

(define exp1 (scan&parse
              "var x = 10
                 in begin set x = 20; x end"
              )                           
  )

(check-equal? (eval-program exp1) 20 "Test 1 failed")


(define exp2 (scan&parse
              "var f = func(x) if (x == 0) { 0 else (x + call f((x - 1))) }
                 in call f(10)"
              )                           
  )

(check-equal? (eval-program exp2) 55 "Test 2 failed")