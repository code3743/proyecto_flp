#lang racket/base
(require rackunit)
(require "../main.rkt")

(define exp1 (scan&parse
              "array (1,2,3,4,5)"
              )                           
  )

(check-equal? (eval-program exp1) #(1 2 3 4 5) "Test 1 failed")

(define exp2 (scan&parse
              "let
                  t = array (1,2,3,4,5)
                  in
                    length(t)"
              )                           
  )

(check-equal? (eval-program exp2) 5 "Test 2 failed")

(define exp3 (scan&parse
              "let
                  t = array (1,2,3,4,5)
                  in
                    index(t, 2)"
              )                           
  )

(check-equal? (eval-program exp3) 3 "Test 3 failed")

(define exp4 (scan&parse
              "let
                  k = array(1,2,3,4,5,6,7,8,9,10)
                  in
                    slice(k, 2, 5)"
              )                           
  )

(check-equal? (eval-program exp4) #(3 4 5 6) "Test 4 failed")

(define exp5 (scan&parse
              "let
                  t = array(1,2,3,4,5)
                  in
                    setlist(t, 2, 10)"
              )                           
  )

(check-equal? (eval-program exp5) #(1 2 10 4 5) "Test 5 failed")