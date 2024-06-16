#lang racket

(provide to-decimal from-decimal)

;*************************************************************************************************
;Conversión de números
;*************************************************************************************************


(define (to-decimal num-str base)
  (cond
    [(eq? base 'b) (string->number (string-replace num-str "b" "") 2)]
    [(eq? base 'o) (string->number (string-replace num-str "0x" "") 8)]
    [(eq? base 'h) (string->number (string-replace num-str "hx" "") 16)]
    ))


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