#lang racket/base
(require rackunit)
(require "../main.rkt")

(define exp1 (scan&parse
              "var
                x = 0
                in
                  begin
                    for i from 0 until 10 by 1 do set x = (x + i);
                    x
                  end"
              )
  )

(check-equal?  (eval-program exp1) 45  "Test 1 failed")

(define exp2 (scan&parse
              "var
                x = 0
                in
                  begin
                    while (x < 10) { set x = (x + 1) };
                    x
                  end"
              )
  )

(check-equal?  (eval-program exp2) 10  "Test 2 failed")

(define exp3 (scan&parse
              "let
                x = 0
                in
                  begin
                    switch (x) {
                      case 1: 1
                      case 2: 2
                      default : 3
                   }
                  end"
              )
  )

(check-equal?  (eval-program exp3) 3  "Test 3 failed")