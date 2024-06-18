#lang racket/base
(require rackunit)
(require "../main.rkt")

(define exp1 (scan&parse
  "let
     s = \"hola mundo cruel\"
   in
     string-length(s)"
))

(check-equal? (eval-program exp1) 16 "Test 1 failed")

(define exp2 (scan&parse
  "let
     a = \"hola\"
     b = \"mundo\"
     c = \"cruel\"
   in
     concat(a, b, c)"
))

(check-equal? (eval-program exp2) "holamundocruel" "Test 2 failed")

(define exp3 (scan&parse
  "let
     s = \"hola mundo cruel\"
     in
       elementAt(s , 5)"
  )
 )

(check-equal? (eval-program exp3) "m" "Test 3 failed")