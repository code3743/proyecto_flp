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
      (a-programa (lstrc body)
                  (cond
                    [(null? lstrc) (eval-expression body (init-env))]
                    [else (
                      let ([structs (map (lambda (strc) (eval-struct strc)) lstrc)])
                       (eval-expression body 
                         (extend-env 
                           (map (lambda (id) (car id)) structs)  (map (lambda (values) (cadr values)) structs)
                            (init-env))
                       )
                    )]
                  )  
      ))))

(define eval-struct 
  (lambda (structs)
    (cases struct-decl structs
      (struct-exp (id lids) 
          (list id lids)
      )
    )
  )
)

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
      (cadena-exp (id1 id2)
        (letrec
            [(crear_string (lambda(lids)
                            (cond
                              [(null? lids) "" ]
                              [else( string-append " " (symbol->string(car lids))(crear_string(cdr lids)))])))]
          (string-append (symbol->string id1)(crear_string id2))
          ))
      (decl-exp (dcl) (
        cases var-decl dcl
          (lvar-exp (ids rands body)
               (let ((args (eval-rands rands env)))
                 (eval-expression body
                                  (extend-env ids args env))))


          (let-exp (ids rands body) 
              (let ((args (eval-rands rands env)))
                (if (contains-set? body)
                    (eopl:error 'decl-exp "Cannot use 'set' in 'let' bindings")
                 (eval-expression body
                                  (extend-env ids args env)))))
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
      (prim-cad-exp (prim lexps)
                     (apply-cadena-primitive prim
                                            (map (lambda (lexps)
                                                   (eval-expression lexps env))
                                                 lexps)))
      (if-exp (test-exp true-exp false-exp) 
              (if (eval-expression test-exp env)
                  (eval-expression true-exp env)
                  (eval-expression false-exp env)))
      (for-exp (cond-exp from-exp until-exp by-exp do-exp)
               (let loop (
                          (env (extend-env (list cond-exp) (list (eval-expression from-exp env)) env))
                          (until-val (eval-expression until-exp env))
                          (by-val (eval-expression by-exp env))
                          (acc 0))
                 (let ((current-val (apply-env env cond-exp)))
                   (if (< current-val until-val) 
                       (let* ((do-val (eval-expression do-exp env))
                              (new-acc (+ acc do-val))
                              (new-env (extend-env (list cond-exp) (list (+ current-val by-val)) env)))
                         (loop new-env until-val by-val new-acc))
                       acc))))
      (while-exp (cond-exp exp) (
        let loop (
          [cond (eval-expression cond-exp env)]
          )
          (if (not cond) #f
              (begin
                (eval-expression exp env)
                (loop (eval-expression cond-exp env))
              )
          )
      ))
      (switch-exp (cond-exp case-exp lexps default-exp) (
        let loop (
          [match-case (eval-expression cond-exp env)]
          [cases (map (lambda (exp) (eval-expression exp env)) lexps)]
          [lexps lexps]
          )
           (cond
            [(null? cases) (eval-expression default-exp env)]
            [(eq? match-case (car cases)) (eval-expression (car lexps) env)]
            [else (loop match-case (cdr cases) (cdr lexps))]
            )
          )
      )
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
      (func-exp (lids exp)
                  (lambda (args env)
                    (let (
                          (new-env (extend-env lids args env))) 
                          (eval-expression exp new-env))
                  ))
      (call-exp (exp args)
                (let (
                      (func-val (eval-expression exp env))
                      (eval-args (map (lambda (arg) (eval-expression arg env)) args)))
                  (apply func-val (list eval-args env))))
      (new-struct-exp (id lexps)
                      (let* (
                             (struct-def (apply-env env id))
                             (struct-field (if struct-def struct-def (eopl:error "Estructura no definida"))))
                        (if (= (length lexps) (length struct-field))
                            (list->vector (list id (map (lambda (exp) (eval-expression exp env)) lexps)))
                            (eopl:error "Numero incorrecto de argumentos"))))
      (get-struct-exp (exp id)
                      (let* (
                             (struct-decl (eval-expression exp env))
                             (struct-id-values (apply-env env (vector-ref struct-decl 0))))
                       
                          (let loop (
                            [field-struct struct-id-values]
                            [values (vector-ref struct-decl 1)])
                        (cond
                          [(null? field-struct) (eopl:error "Campo no encontrado") ]
                          [(eq? (car field-struct) id) (car values)]
                          [else (loop (cdr field-struct) (cdr values))]
                        ))
                        
                      )) 
      (set-struct-exp (exp1 id exp2)
                    (let* 
                          (
                           [struct-def (eval-expression exp1 env)]
                           [struct-id-values (apply-env env (vector-ref struct-def 0))]
                           [values (vector-ref struct-def 1)])
                          (vector-set!
                              struct-def
                              1
                              (let loop(
                                  [struct-values struct-id-values]
                                  [values values]
                                  [acc '()]
                                )
                                (cond
                                  [(null? struct-values) (eopl:error "Campo no encontrado")]
                                  [(eq? id (car struct-values)) (append acc (list (eval-expression exp2 env)))]
                                  [else (loop (cdr struct-values) (cdr values) (car values))]
                                )
                              )
                          ))
      )
      (match-exp (exp rexps lexps) 
                  (let 
                     loop([regular  (map (lambda (exp) (apply-regular-exp exp)) rexps)]
                          [default-match (list #f )]
                          [match-exps lexps]
                          [match-value (eval-expression exp env)])
                          (cond
                            [(and (null? regular) (not (car default-match))) (eopl:error "No hay match")]
                            [(null? regular) (eval-expression (cadr default-match) env)]
                            [(is-number? match-value) (if (eq? (caar regular)'num-match) 
                                                          (eval-expression (car match-exps) (extend-env (list (cadar regular)) (list match-value) env))
                                                          (loop (cdr regular) (defaul-match-value default-match (caar regular) (car match-exps)) (cdr match-exps) match-value)
                                                      )]
                            [(string? match-value)  (if (eq? (caar regular)'cad-match) 
                                                        (eval-expression (car match-exps) (extend-env (list (cadar regular)) (list match-value) env))
                                                        (loop (cdr regular) (defaul-match-value default-match (caar regular) (car match-exps)) (cdr match-exps) match-value)
                                                      )]
                            [(boolean? match-value) (if (eq? (caar regular)'bool-match)
                                                        (eval-expression (car match-exps) (extend-env (list (cadar regular)) (list match-value) env))
                                                        (loop (cdr regular) (defaul-match-value default-match (caar regular) (car match-exps)) (cdr match-exps) match-value)
                                                      )]
                            [(null? match-value) (if(eq? (caar regular )'empty-match) 
                                                    (eval-expression (car match-exps) env)
                                                    (loop (cdr regular) (defaul-match-value default-match (caar regular) (car match-exps)) (cdr match-exps) match-value)
                                                      )]
                            [(list? match-value) (if (eq? (caar regular )'list-match)
                                                     (eval-expression (car match-exps) (extend-env (cadar regular) (list (car match-value) (cdr match-value)) env))
                                                     (loop (cdr regular) (defaul-match-value default-match (caar regular) (car match-exps)) (cdr match-exps) match-value)
                                                      )]
                            [(vector? match-value) (if (eq? (caar regular) 'array-math)
                                                        (if (<= (length (cadar regular)) (vector-length match-value)) 
                                                          (eval-expression (car match-exps) (extend-env (cadar regular) (vector->list (array-slice match-value 0 (- (length (cadar regular)) 1 ))) env))
                                                          (eopl:error "Numero incorrecto de argumentos")
                                                        )
                                                        (loop (cdr regular) (defaul-match-value default-match (caar regular) (car match-exps)) (cdr match-exps) match-value)
                                                        )]
                            [else (loop (cdr regular) (defaul-match-value default-match (caar regular) (car match-exps)) (cdr match-exps) match-value)]
                          ))
                  )
      )
      
    )
)

(define defaul-match-value
  (lambda (current-value regular exp )
    (cond
      [(eq? regular 'default-match) (list #t  exp)]
      [(car current-value) current-value ]
      [else (list #f )]
    ) 
  )
)

(define apply-regular-exp 
  (lambda (regular)
    (cases regular-exp regular
      (list-match-exp (id1 id2) (list 'list-match (list id1 id2)))
      (num-match-exp (id) (list 'num-match id))
      (cad-match-exp (id) (list 'cad-match id))
      (bool-match-exp (id) (list 'bool-match id))
      (array-match-exp (ids) (list 'array-math ids))
      (empty-match-exp () '(empty-match))
      (default-match-exp () '(default-match))
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

(define apply-cadena-primitive
  (lambda (prim args)
      (cases primitivaCadena prim
        (concat-primCad() (apply string-append args))
        (length-primCad() (string-length (car args)))
        (index-primCad() (string (string-ref (car args)(cadr args)))))))

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

; Detects 'set' operation when used with 'let'
(define contains-set?
  (lambda (expr)
    (cases expresion expr
      (set-exp (id rhs-exp) #t)
      (begin-exp (id rhs-exp) (contains-set? id))
      (for-exp (cond-exp from-exp until-exp by-exp do-exp) (contains-set? do-exp))
      (while-exp (cond-exp exp) (contains-set? exp))
      (else #f))))

(interpretador)
