#lang racket

(provide to-decimal from-decimal convert-string)

;*************************************************************************************************
;Conversión de números
;*************************************************************************************************


(define (to-decimal num base)
  (cond
    [(eq? base 'b) (string->number (string-replace (to-string num) "b" "") 2)]
    [(eq? base 'o) (string->number (string-replace (to-string num) "0x" "") 8)]
    [(eq? base 'h) (string->number (string-replace (to-string num) "hx" "") 16)]
    )
)


(define (from-decimal num base)
  (convert-string (cond
    [(eq? base 'b) (number->string num 2)]
    [(eq? base 'o) (number->string num 8)]
    [(eq? base 'h) (string-upcase (number->string num 16))]
    )))


(define (string-contains-letters? str)
  (regexp-match? #rx"[A-Za-z]" str))

(define (convert-string str)
  (if (string-contains-letters? str)
      (string->symbol str)
      (string->number str)))


(define to-string (lambda (x) (cond
  [(string? x) x]
  [(symbol? x) (symbol->string x)]
  [(number? x) (number->string x)]
  )))