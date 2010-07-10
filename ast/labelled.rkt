#lang racket

(require "predexpr.rkt"
         "action.rkt"
         "../params.rkt"
         srfi/13)
        
(define-struct Labelled
  (label struct)
  #:property prop:custom-write 
  (lambda (struct port write?)
    (pp-Labelled struct port)))

(define (pp-Labelled struct (port (current-output-port)))
  (when (or (print-debug?) (not (auto-label? (Labelled-label struct))))
    (fprintf port "@~a " (Labelled-label struct)))
  (fprintf port "~a" (Labelled-struct struct)))

(define (labelled-predicate? u)
  (and (Labelled? u)
       (predicate? (Labelled-struct u))))

(define (labelled-typed-predicate? u)
  (and (Labelled? u)
       (typed-predicate? (Labelled-struct u))))

(define (labelled-action? u)
  (and (Labelled? u)
       (action? (Labelled-struct u))))

(define (make-Labelled/auto val) (make-Labelled (make-auto-label) val))

(define (make-auto-label) (gensym 'auto-label))

(define (auto-label? s)
  (string-prefix? "auto-label" (symbol->string s)))

(define (Labelled/auto? l)
  (auto-label? (Labelled-label l)))

(provide/contract
 [struct Labelled ((label symbol?) (struct (or/c predicate? action?)))]
 [labelled-predicate? (any/c . -> . boolean?)]
 [labelled-typed-predicate? (any/c . -> . boolean?)]
 [labelled-action? (any/c . -> . boolean?)]
 [make-Labelled/auto ((or/c predicate? action?) . -> . Labelled?)]
 [Labelled/auto? (Labelled? . -> . boolean?)])