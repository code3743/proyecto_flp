#lang eopl
(require "grammar.rkt")
(require "environment.rkt")
(require "utils.rkt")


(sllgen:make-define-datatypes lexica gramatica)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes lexica gramatica)))

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser lexica gramatica))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner lexica gramatica))


;El Interpretador (FrontEnd + Evaluación + señal para lectura +
(define interpretador
  (sllgen:make-rep-loop "--> "
    (lambda (pgm) (eval-program  pgm))
    (sllgen:make-stream-parser 
      lexica
      gramatica)))

;eval-program: <program> ->numero
(define eval-program
  (lambda (pgm)
    (cases programa pgm
      (a-programa (str body)
                  (eval-expression body (init-env))))))

;eval-expression: <expression> <enviroment> -> numero
(define eval-expression
  (lambda (exp env)
    (cases expresion exp
      (bool-exp (bool) 
        (cases bool-expresion bool
          (true-exp () #T)
          (false-exp () #F)
          )
        )
      (var-exp (id) (apply-env env id))
      (num-exp (num) 
        (cases numero-exp num
          (decimal-num (num) num)
          (octal-num (num) (convert-string num))
          (bin-num (num) (convert-string num))
          (hex-num (num) (convert-string num))
          (float-num (num) num)
        )
      )
      (cadena-exp (id1 id2) '())
      (decl-exp (dcl) (
        cases var-decl dcl
          (lvar-exp (ids rands body)
               (let ((args (eval-rands rands env)))
                 (eval-expression body
                                  (extend-env ids args env))))


          (let-exp (ids rands body) 
              (let ((args (eval-rands rands env)))
                 (eval-expression body
                                  (extend-env ids args env))))
          )
      )
      (lista-exp(lexps) (
        map (lambda (exp) (eval-expression exp env)) lexps
      ))
      (cons-exp (exp1 exp2) 
        (cons (eval-expression exp1 env) (eval-expression exp2 env)))
      (empty-list-exp () '())
      (array-exp (lexp)
                 (let (
                       (evaluated (map (lambda (exp) (eval-expression exp env)) lexp)))
                       (list->vector evaluated)))
      (prim-num-exp (exp1 prim exp2) (
        apply-num-primitive prim (eval-expression exp1 env) (eval-expression exp2 env))
      )
      (prim-bool-exp (prim lexps)
            (apply-bool-primitive prim 
              (map (lambda (exp) (let ([value (eval-expression exp env)])
                (if (boolean? value) value
                (eopl:error 'apply-bool-primitive "Non-boolean argument"))
              )) lexps)
            )
      )
      (prim-list-exp (prim exp) 
       (apply-list-primitive prim (eval-expression exp env)) 
      )
      (prim-array-exp (prim lexps)
                      (let (
                            (args (eval-rands lexps env)))
                        (apply-array-primitive prim args)))
      (prim-cad-exp (prim lexps) '())
      (if-exp (test-exp true-exp false-exp) '())
      (for-exp (cond-exp from-exp until-exp by-exp do-exp) '())
      (while-exp (cond-exp exp) '())
      (switch-exp (cond-exp case-exp lexps default-exp) '())
      (begin-exp (exp exps) 
                 (let loop (
                            (acc (eval-expression exp env))
                             (exps exps))
                    (if (null? exps) acc
                        (loop (eval-expression (car exps) env)
                              (cdr exps)))))
      (set-exp (id rhs-exp)
               (begin
                 (setref!
                  (apply-env-ref env id)
                  (eval-expression rhs-exp env))
                 1))
      (func-exp (lids exp) '())
      (call-exp (exp lexps) '())
      (new-struct-exp (id lexps) '())
      (get-struct-exp (exp id) '())
      (set-struct-exp (exp1 id exp2) '())
      (match-exp (exp rexps lexps) '())
      )
    )
)

(define apply-num-primitive
  (lambda (prim num1 num2)
    (cases primitiva prim
      (sum-prim () (operation-numerical + num1 num2 #F))
      (minus-prim () (operation-numerical - num1 num2 #F))
      (mult-prim () (operation-numerical * num1 num2 #F))
      (mod-prim () (operation-numerical modulo num1 num2 #F) )
      (elevar-prim () (operation-numerical expt num1 num2 #F ))
      (menor-prim () (operation-numerical < num1 num2 #T))
      (mayor-prim () (operation-numerical > num1 num2 #T))
      (menorigual-prim () (operation-numerical <= num1 num2 #T))
      (mayorigual-prim () (operation-numerical >= num1 num2 #T))
      (diferente-prim () (not (operation-numerical eq? num1 num2 #T)))
      (igual-prim () (operation-numerical eq? num1 num2 #T))
      )
  )
)


(define apply-bool-primitive
  (lambda (prim args)
    (cases primitivaBooleana prim
      (and-prim () (let loop ([args args])
                    (if (null? args) #t
                        (if (not (car args)) #f (loop (cdr args))))))
      (or-prim () (let loop ([args args])
          (if (null? args) #f
              (if (car args) #t (loop (cdr args)))
          )))
      (xor-prim () (and (or (car args) (cadr args)) (not (and (car args) (cadr args)))))
      (not-prim () (not (car args)))
      )
    )
)

(define apply-list-primitive
  (lambda (prim args)
    (cases primitivaListas prim
      (first-primList () (car args))
      (rest-primList () (cdr args))
      (empty-primList () (null? args))
    )
  )
)

(define apply-array-primitive
  (lambda (prim args)
    (cases primitivaArray prim
      (length-primArr () (vector-length (car args)))
      (index-primArr () (let (
                              (arr (car args))
                              (index (cadr args)))
                           (vector-ref arr index)))
      (slice-primArr() (let (
                             (vec (car args))
                             (start (cadr args))
                             (end (caddr args)))
                         (array-slice vec start end)))
      (setlist-primArr () (let (
                                (vec (car args))
                                (pos (cadr args))
                                (val (caddr args)))
                            (vector-set! vec pos val)
                             vec))
    )
  )
)


(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))



(interpretador)
