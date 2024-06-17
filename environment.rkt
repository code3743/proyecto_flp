;; FILEPATH: ../proyecto_flp/environment.rkt
#lang eopl

(provide (all-defined-out))

;; Represents an environment in the form of a record.
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?)) ; List of symbols
   (vec vector?) ; Vector of values
   (env environment?))) ; Reference to the parent environment

;; Represents a reference to a value in the environment.
(define-datatype reference reference?
  (a-ref (position integer?) ; Position of the value in the vector
         (vec vector?))) ; Vector containing the values

;; Creates an empty environment.
(define empty-env
  (lambda ()
    (empty-env-record)))

;; Extends the environment by adding symbols and their corresponding values.
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))

;; Applies the environment reference to a symbol and returns the corresponding value.
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

;; Dereferences a reference and returns the value it refers to.
(define deref
  (lambda (ref)
    (primitive-deref ref)))

;; Dereferences a reference and returns the value it refers to.
(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

;; Sets the value of a reference.
(define setref!
  (lambda (ref val)
    (primitive-setref! ref val)))

;; Sets the value of a reference.
(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
             (vector-set! vec pos val)))))

;; Finds the position of a symbol in a list.
(define rib-find-position 
  (lambda (sym los)
    (list-find-position sym los)))

;; Finds the position of a symbol in a list.
(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

;; Finds the index of an element in a list.
(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

;; Applies the environment to a symbol and returns the corresponding value.
(define apply-env
  (lambda (env sym)
    (deref (apply-env-ref env sym))))

;; Initializes the environment with predefined symbols and values.
(define init-env
  (lambda ()
    (extend-env
     '(x y z) ; Symbols
     '(1 2 3) ; Values
     (empty-env))))