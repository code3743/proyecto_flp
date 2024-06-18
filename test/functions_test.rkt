#lang racket/base
(require rackunit)
(require "../main.rkt")

(define exp1 (scan&parse
              "var
                f = func(x) x
                in
                  call f(10)"
              )
  )

(check-equal?  (eval-program exp1) 10  "Test 1 failed")

(define exp2 (scan&parse
              "var
                x = 10 f = func(a) (a + x)
                in
                  let x = 30 in call f(10)"
              )
  )

(check-equal?  (eval-program exp2) 40  "Test 2 failed")
