#lang scheme

(require "../ast.scm"
         "prop.scm")

;; This is the main entry point for the SAT-based encoding
;; of predicates and expressions and the operational semantics
;; behind Event-B

(define-struct SAT-Predicate
  (constraints bvars pred))

(define-struct SAT-Expression
  (constraints bvars result expr))

(provide/contract
 [encode-predicate (predicate? . -> . SAT-Predicate)]
 [encode-expression (expression? . -> . SAT-Expression)]
 
 [struct SAT-Predicate ((constraints (listof Prop-Predicate?))
                        (bvars (listof Prop-Variable?))
                        (pred predicate?))]
 [struct SAT-Expression ((constraints (listof Prop-Predicate?))
                         (bvars (listof Prop-Variable?))
                         (expr expression?))])
