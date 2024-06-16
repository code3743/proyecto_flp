#lang eopl
(require "utils.rkt")

(define lexica
'((white-sp
   (whitespace) skip)
  (comment
   ("//" (arbno (not #\newline))) skip)
  (identificador
   (letter (arbno (or letter digit "?"))) symbol)
  (digitoBinario
   ("b" (or "0" "1") (arbno (or "0" "1"))) string)
  (digitoBinario
   ("-" "b" (or "0" "1") (arbno (or "0" "1"))) string)
  (digitoDecimal
   (digit (arbno digit)) number)
  (digitoDecimal
   ("-" digit (arbno digit)) number)
  (digitoOctal
   ("0x" (or "0" "1" "2" "3" "4" "5" "6" "7")(arbno (or "0" "1" "2" "3" "4" "5" "6" "7"))) string)
  (digitoOctal
   ("-" "0x" (or "0" "1" "2" "3" "4" "5" "6" "7") (arbno (or "0" "1" "2" "3" "4" "5" "6" "7"))) string)
  (digitoHexadecimal
   ("hx" (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F") (arbno (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F"))) string)
  (digitoHexadecimal
   ("-" "hx" (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F") (arbno (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F"))) string) 
  (flotante
   (digit (arbno digit) "." digit (arbno digit)) number)
  (flotante
   ("-" digit (arbno digit) "." digit (arbno digit)) number)
  ))

(define gramatica
  '(
    (programa ((arbno struct-decl) expresion) a-programa)
    (expresion (bool-expresion) bool-exp)
    (expresion (identificador) var-exp)
    (expresion (numero-exp) num-exp)    
    (expresion ("\"" identificador (arbno identificador) "\"") cadena-exp)
    (expresion (var-decl) decl-exp)

    ;;Listas y arrays
    (expresion ("list" "(" (separated-list expresion ",") ")") lista-exp)
    (expresion ("cons" "(" expresion expresion ")") cons-exp)
    (expresion ("empty") empty-list-exp)
    (expresion ("array" "(" (separated-list expresion ",") ")") array-exp)

    ;;Expresion primitivas
    ;;Primitiva numerica
    (expresion ("(" expresion primitiva expresion ")") prim-num-exp)
    ;;Primitiva booleana
    (expresion (primitivaBooleana "(" (separated-list expresion ",") ")") prim-bool-exp)
    ;;Primitiva listas
    (expresion (primitivaListas "(" expresion ")") prim-list-exp)
    ;;Primitiva array
    (expresion (primitivaArray "(" (separated-list expresion ",") ")") prim-array-exp)
    ;;Primitiva de cadenas
    (expresion (primitivaCadena "(" (separated-list expresion ",") ")") prim-cad-exp)


    ;;Condicionales
    (expresion ("if" expresion "{" expresion "else" expresion "}") if-exp)


    ;;Iteradores
    (expresion ("for" identificador "from" expresion "until" expresion "by" expresion "do" expresion) for-exp)
    (expresion ("while" expresion "{" expresion "}") while-exp)

    ;;Switch
    (expresion ("switch" "(" expresion ")" "{" (arbno "case" expresion ":" expresion) "default" ":" expresion "}") switch-exp)

    ;;Secuenciación y asignación
    (expresion ("begin" expresion (arbno ";" expresion) "end") begin-exp)
    (expresion ("set" identificador "=" expresion) set-exp)

    ;;Funciones
    (expresion ("func" "(" (separated-list identificador ",") ")" expresion) func-exp)
    (expresion ("call" expresion "(" (separated-list expresion ",") ")") call-exp)

    ;;Instanciación y uso de estructuras
    (expresion ("new" identificador "(" (separated-list expresion ",") ")") new-struct-exp)
    (expresion ("get" expresion "." identificador) get-struct-exp)
    (expresion ("set-struct" expresion "." identificador "=" expresion) set-struct-exp)

    ;;Reconocimiento de patrones
    (expresion ("match" expresion "{" (arbno regular-exp "=>" expresion) "}") match-exp)

    ;;Numero-exp
    (numero-exp (digitoDecimal) decimal-num)
    (numero-exp (digitoOctal) octal-num)
    (numero-exp (digitoBinario) bin-num)
    (numero-exp (digitoHexadecimal) hex-num)
    (numero-exp (flotante) float-num)
    
    ;;Bool-exp
    (bool-expresion ("true") true-exp)
    (bool-expresion ("false") false-exp)

    ;;primitivas numéricas
    (primitiva ("+") sum-prim)
    (primitiva ("-") minus-prim)
    (primitiva ("*") mult-prim)
    (primitiva ("mod") mod-prim)
    (primitiva ("pow") elevar-prim)
    (primitiva ("<") menor-prim)
    (primitiva (">") mayor-prim)
    (primitiva ("<=") menorigual-prim)
    (primitiva (">=") mayorigual-prim)
    (primitiva ("!=") diferente-prim)
    (primitiva ("==") igual-prim)

    ;;primitiva booleana
    (primitivaBooleana ("and") and-prim)
    (primitivaBooleana ("or") or-prim)
    (primitivaBooleana ("xor") xor-prim)
    (primitivaBooleana ("not") not-prim)

    ;;Primitiva listas
    (primitivaListas ("first") first-primList)
    (primitivaListas ("rest") rest-primList)
    (primitivaListas ("empty?") empty-primList)

    ;;Primitiva arrays
    (primitivaArray ("length") length-primArr)
    (primitivaArray ("index") index-primArr)
    (primitivaArray ("slice") slice-primArr)
    (primitivaArray ("setlist") setlist-primArr)

    ;;Primitiva cadenas
    (primitivaCadena ("concat") concat-primCad)
    (primitivaCadena ("string-length") length-primCad)
    (primitivaCadena ("elementAt") index-primCad)
    
    ;;Variables
    (var-decl ("var" (arbno identificador "=" expresion) "in" expresion) lvar-exp)
    (var-decl ("let" (arbno identificador "=" expresion) "in" expresion) let-exp)
    
    ;;Estructuras de datos
    (struct-decl ("struct" identificador "{" (arbno identificador) "}") struct-exp)

    ;;Expresiones regulares
    (regular-exp (identificador "::" identificador) list-match-exp)
    (regular-exp ("numero" "(" identificador ")") num-match-exp)
    (regular-exp ("cadena" "(" identificador ")") cad-match-exp)
    (regular-exp ("boolean" "(" identificador ")") bool-match-exp)
    (regular-exp ("array" "(" (separated-list identificador ",") ")") array-match-exp)
    (regular-exp ("empty") empty-match-exp)
    (regular-exp ("default") default-match-exp)
    )
  )

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

;Ambiente inicial
(define init-env
  (lambda ()
    (extend-env
     '(x y z)
     '(1 2 3)
     (empty-env))))

;Ambientes
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vec vector?)
   (env environment?)))

;empty-env
(define empty-env
  (lambda ()
    (empty-env-record)))

;extended-env
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))

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
      (decl-exp (dcl) '())
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
        (if (null? lexps) (eopl:error 'apply-bool-primitive "No arguments given")
            (apply-bool-primitive prim 
              (map (lambda (exp) (let ([value (eval-expression exp env)])
                (if (boolean? value) value
                (eopl:error 'apply-bool-primitive "Non-boolean argument"))
              )) lexps)
            )
        )
      )
      (prim-list-exp (prim exp) 
        (let ([value (eval-expression exp env)])
          (if (list? value) 
            (apply-list-primitive prim value)
            (eopl:error 'apply-list-primitive "Non-list argument"))
        )
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

(define apply-env
  (lambda (env sym)
    (deref (apply-env-ref env sym))))

(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env-ref "No binding for ~s" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (rib-find-position sym syms)))
                             (if (number? pos)
                                 (a-ref pos vals)
                                 (apply-env-ref env sym)))))))

(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)))

(define deref
  (lambda (ref)
    (primitive-deref ref)))

(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

(define setref!
  (lambda (ref val)
    (primitive-setref! ref val)))

(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
             (vector-set! vec pos val)))))

(define rib-find-position 
  (lambda (sym los)
    (list-find-position sym los)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

(interpretador)
