#lang racket/base
(require rackunit)
(require "../main.rkt")

(define exp1 (scan&parse
              "struct perro {nombre edad color}
              let
                t = new perro (\"lucas\", 10, \"verde\")
                in get t.nombre"
                                )
  )

(check-equal? (eval-program exp1) "lucas" "Test 1 failed")

(define exp2 (scan&parse
              "struct perro {nombre edad color}
              let
                t = new perro (\"lucas\", 10, \"verde\")
                in begin
                    set-struct t.nombre = \"pepe\";
                    get t.nombre
                  end"
                                )
  )

(check-equal? (eval-program exp2) "pepe" "Test 2 failed")

(define exp3 (scan&parse
              "struct perro {nombre edad color}
              let
                t = new perro (\"lucas\", 10, \"verde\")
                in 
                    set-struct t.nombre = \"pepe\" "
                                )
  )

(check-equal? (void? (eval-program exp3)) #t "Test 3 failed")


