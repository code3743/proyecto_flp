#lang racket/base
(require rackunit)
(require "../main.rkt")

(define exp1 (scan&parse
  "let
     x = list(1,2,3,4 ,5)
     in
       match x {
         x :: xs => xs
         default => 0
       }"
))

(check-equal? (eval-program exp1) '(2 3 4 5) "Test 1 failed")

(define exp2 (scan&parse
  "let
     x = 10
     in
       match x {
         numero(x) => x
         default => 0
       }"
))

(check-equal? (eval-program exp2) 10 "Test 2 failed")

(define exp3 (scan&parse
  "let
     x = \"hola mundo\"
     in
       match x {
         cadena(x) => x
         default => 0
       }"
          ))
(check-equal? (eval-program exp3) "hola mundo" "Test 3 failed")

(define exp4 (scan&parse
  "let
     x = true
     in
       match x {
         boolean(x) => x
         default => 0
       }"
          ))
(check-equal? (eval-program exp4) #t "Test 4 failed")

(define exp5 (scan&parse
  "let
     x = array(1,2,3 ,4 ,5)
     in
       match x {
         array(x,y,z) => list(x,y,z)
         default => 0
       }"
          ))
(check-equal? (eval-program exp5) '(1 2 3) "Test 5 failed")

(define exp6 (scan&parse
  " let
      f = func(x)
        match x {
          empty => 0
          x :: xs => (x + call f(xs))
        }
      in
      call f(list(1 ,2 ,3 ,4 ,5))"
          ))
(check-equal? (eval-program exp6) 15 "Test 6 failed")





