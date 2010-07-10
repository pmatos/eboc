#lang scheme

;; Implementation of structures and function utilities for 
;; Propositional Logic

(define-struct Prop-Predicate ())

(define-struct (Prop-Leqv Prop-Predicate)
  (arg1 arg2))
(define-struct (Prop-Limp Prop-Predicate)
  (arg1 arg2))
(define-struct (Prop-Land Prop-Predicate)
  (arg1 arg2))
(define-struct (Prop-Lor Prop-Predicate)
  (arg1 arg2))
(define-struct (Prop-Lnot Prop-Predicate)
  (arg))
(define-struct (Prop-Variable Prop-Predicate)
  (name))

;; From a list of symbols, it creates a function Prop-Predicate
(define (make-pp lst)
  (match lst
    ((? symbol?) (make-Prop-Variable lst))
    (`(land ,arg1 ,arg2)
     (make-Prop-Land (make-pp arg1) (make-pp arg2)))
    (`(lor ,arg1 ,arg2)
     (make-Prop-Lor (make-pp arg1) (make-pp arg2)))
    (`(limp ,arg1 ,arg2) 
     (make-Prop-Limp (make-pp arg1) (make-pp arg2)))
    (`(leqv ,arg1 ,arg2)
     (make-Prop-Leqv (make-pp arg1) (make-pp arg2)))
    (`(lnot ,arg)
     (make-Prop-Lnot (make-pp arg)))))
     
;; Contracts
(define (deep-symbol-list/c u)
  (or (symbol? u)
      (and (list? u)
           (andmap deep-symbol-list/c u))))

(provide Prop-Predicate?)
(provide/contract
 [make-pp (deep-symbol-list/c . -> . Prop-Predicate)])


