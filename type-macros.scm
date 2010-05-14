#lang scheme

(require (except-in "types.scm" t))

(provide typing-rules)

;; Type Macros
(define-syntax t
  (syntax-rules (P x : Z B)
    [(_ Z) (make-Type-Integer)]
    [(_ B) (make-Type-Boolean)]
    [(_ (P a)) (make-Type-Powerset (t a))]
    [(_ (a x b)) (make-Type-CartesianProduct (t a) (t b))]
    [(_ (name : (el1 els ...))) (make-Type-Enumeration name (cons el1 els ...))]
    [(_ a) (make-Type-Polymorphic a)]))

(define-for-syntax (expand-rules stx)
  (syntax-case stx (rule)
    [(rule (ops ...) (vars ...) ((lhs == rhs) ...) return-type)
     #'((ops ...)
        (let ([vars (make-fresh-type-variable-name)] ...)
          (values 
           (map make-Type-Polymorphic (list vars ...))
           (list (cons lhs (t rhs)) ...)
           (t return-type))))]))

(define-syntax (typing-rules stx)
  (syntax-case stx ()
    [(_ name (types ...) rules ...)
     #'(identifier? name)
     (with-syntax ([(branch ...) (map expand-rules (syntax->list #'(rules ...)))])
       #'(define name
           (lambda (op types ...)
             (case op branch ...))))]))