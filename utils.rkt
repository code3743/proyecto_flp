#lang racket

(provide to-decimal from-decimal)


(define string-upcase
  (lambda (str)
    (list->string (map char-upcase (string->list str)))))


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
  (string->symbol (cond
    [(eq? base 'b) (number->string num 2)]
    [(eq? base 'o) (number->string num 8)]
    [(eq? base 'h) (string-upcase (number->string num 16))]
    )))
