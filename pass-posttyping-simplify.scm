#lang scheme

(require "ast.scm")

;; This is a post typing simplification pass.

(provide (rename-out (pass-machine typed-machine/simplify)))

;; The only thing it does is remove redundant invariants and axioms
;; that only provide typing information.
;; After typing has happened predicates as follows can be removed.
;; id : <BasicType>
;; id <: <BasicType>
;; They don't add anything to the model besides the typing information.
;; A predicate is remove if it is considered redundant. 
;; The redundancy check is performed by predicate-is-for-type?

(define (predicate-is-for-type? lpred)
  (match lpred
    ((struct Labelled (_ 
                       (struct Predicate-RelOp ('in (? Identifier?) (? type-expression?)))))
     #t)
    ((struct Labelled (_
                       (struct Predicate-RelOp ('subseteq (? Identifier?) (? type-expression?)))))
     #t)
    (else #f)))

(define (pass-machine mach)
  (match mach
    ((struct Machine (name sees vars events invariants variants theorems inits))
     (make-Machine name (map pass-context sees) vars 
                   (map pass-event events)
                   (filter (lambda (i) (not (predicate-is-for-type? i))) invariants)
                   variants
                   theorems
                   inits))))

(define (pass-context ctx)
  (match ctx
    ((struct Context (name extensions-list sets-list constants-list axioms-list theorems-list))
     (make-Context name (map pass-context extensions-list) sets-list constants-list
                   (filter (lambda (a) (not (predicate-is-for-type? a))) axioms-list)
                   theorems-list))))
                   
(define (pass-event ev)
  (match ev
    ((struct Event (name status locals guards actions))
     (make-Event name status locals 
                 (filter (lambda (g) (not (predicate-is-for-type? g))) guards)
                 actions))))
