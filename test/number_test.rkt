#lang racket/base
(require rackunit)
(require "../main.rkt")

(define exp1 (scan&parse
    "let
      a = 1
      in
        a
    "
  )
)

(check-equal?  (eval-program exp1) 1  "Test 1 failed")


(define exp2 (scan&parse
    "let
      a = 3.1416
      in
        a
    "
  )
)

(check-equal?  (eval-program exp2) 3.1416 "Test 2 failed")

(define exp3 (scan&parse
    "let
      a = b1010001
      in
        a
    "
  )
)

(check-equal?  (eval-program exp3) 'b1010001 "Test 3 failed")

(define exp4 (scan&parse
    "let
      a = -0x51
      in
        a
    "
  )
)

(check-equal?  (eval-program exp4) '-0x51 "Test 4 failed")

(define exp5 (scan&parse
    "let
      a = hxA3FF
      in
        a
    "
  )
)

(check-equal?  (eval-program exp5) 'hxA3FF "Test 5 failed")

(define exp6 (scan&parse
    "(b1000 + b10)"
  )
)

(check-equal?  (eval-program exp6) 'b1010 "Test 6 failed")

(define exp7 (scan&parse
    "(0x77 + 0x3)"
  )
)

(check-equal?  (eval-program exp7) '0x102 "Test 7 failed")

(define exp8 (scan&parse
    "(123.2 - 1.2)"
  )
)

(check-equal?  (eval-program exp8) 122.0 "Test 8 failed")

(define exp9 (scan&parse
    "(hx100 - hx2)"
  )
)

(check-equal?  (eval-program exp9) 'hxFE "Test 9 failed")




