;; FILEPATH: ../proyecto_flp/utils.rkt
#lang racket

;; Provides the following functions:
;; - to-decimal: Converts a number from a given base to decimal.
;; - from-decimal: Converts a decimal number to a given base.
;; - convert-string: Converts a string to a number or symbol.
;; - operation-numerical: Performs a numerical operation on two numbers.
;; - array-slice: Extracts a slice from an array.

(provide to-decimal from-decimal convert-string operation-numerical array-slice)

;; Converts a number from a given base to decimal.
(define (to-decimal num base)
  (cond
    [(eq? base 'd) num]
    [(eq? base 'b) (string->number (string-replace (to-string num) "b" "") 2)]
    [(eq? base 'o) (string->number (string-replace (to-string num) "0x" "") 8)]
    [(eq? base 'h) (string->number (string-replace (to-string num) "hx" "") 16)]
    )
)

;; Converts a decimal number to a given base.
(define (from-decimal num base)
  (convert-string  (prefix-base base (cond
    [(eq? base 'd) (number->string num)]
    [(eq? base 'b) (number->string num 2)]
    [(eq? base 'o) (number->string num 8)]
    [(eq? base 'h) (string-upcase (number->string num 16))]
    )))
)

;; Checks if a string contains letters.
(define (string-contains-letters? str)
  (regexp-match? #rx"[A-Za-z]" str))

;; Converts a string to a number or symbol.
(define (convert-string str)
  (if (string-contains-letters? str)
      (string->symbol str)
      (string->number str)))

;; Helper function to add the prefix for a given base.
(define _prefix-base (lambda (base)
  (cond
    [(eq? base 'd) ""]
    [(eq? base 'b) "b"]
    [(eq? base 'o) "0x"]
    [(eq? base 'h) "hx"]
    )
  )
)

;; Adds the prefix for a given base to a number.
(define prefix-base (lambda (base num)
  (if (string-contains? num "-")
    (string-append "-" (_prefix-base base) (string-replace num "-" ""))
    (string-append (_prefix-base base) num)
  ))  
)

;; Converts a value to a string representation.
(define to-string (lambda (x) (cond
  [(string? x) x]
  [(symbol? x) (symbol->string x)]
  [(number? x) (number->string x)]
  )))

;; Extracts the base from a string representation of a number.
(define extract-base (lambda (str)
  (cond
    [(string-contains? str "b") 'b]
    [(string-contains? str "0x") 'o]
    [(string-contains? str "hx") 'h]
    [else 'd]
    ))
)

;; Performs a numerical operation on two numbers.
(define operation-numerical 
  (lambda (op num1 num2 boolean?)
    (let ([base (extract-base (to-string num1))]
          [num1 (to-decimal num1 (extract-base (to-string num1)))]
          [num2 (to-decimal num2 (extract-base (to-string num2)))]
          )
      (if boolean?
          (op num1 num2)
          (from-decimal (op num1 num2) base)
       )
    )
  )
)

;; Extracts a slice from an array.
(define array-slice
  (lambda (v start end)
    (let (
          (len (+ 1 (- end start))))
      (build-vector len (lambda (i) (vector-ref v (+ i start)))))))
