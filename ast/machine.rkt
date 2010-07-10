#lang racket

(require "context.rkt"
         "predexpr.rkt"
         "event.rkt"
         "labelled.rkt"
         "action.rkt"
         "../params.rkt"
         "../untyped-utils.rkt")

(provide pp-Machine)

(define-struct Machine
  (name sees vars events invariants variants theorems initialisation)
  #:property prop:custom-write
  (lambda (struct port write?)
    (pp-Machine struct port)))

(define (machine-seen-constants mch)
  (append-map context-seen-constants (Machine-sees mch)))

(define (machine-seen-axioms mch)
  (append-map context-seen-axioms (Machine-sees mch)))

(define (machine-seen-sets mch)
  (append-map context-seen-sets (Machine-sees mch)))

(define (pp-Machine struct (port (current-output-port)))
  (parameterize ([current-output-port port])
    (letrec ([indent (lambda (n) (printf (make-string (* (spaces/tab) n) #\space)))])
      (match struct
        ((struct Machine (name sees vars events invs variants thms inits))
         (printf "Machine ~a~n" name)
         
         (unless (null? sees)
           (indent 1) (printf "Sees~n")
           (for-each (lambda (cname) (indent 2) (printf "~a~n" cname)) (map Context-name sees)))
         
         (unless (null? vars)
           (indent 1) (printf "Variables~n")
           (for-each (lambda (var) 
                       (indent 2) 
                       (if (typed-variable? var)
                           (if (print-debug?)
                               (printf "~a oftype ~a~n"
                                       (Expr/wt-expr var)
                                       (Expr/wt-type var))
                               (printf "~a~n" (Expr/wt-expr var)))
                           (printf "~a~n" var)))
                     vars))
         
          (unless (null? invs)
           (indent 1) (printf "Invariants~n")
           (for-each (lambda (inv) (indent 2) (printf "~a~n" inv))
                     invs))
         
         (unless (null? inits)
           (indent 1) (printf "Initialisation~n")
           (for-each (lambda (init) (indent 2) (printf "~a~n" init)) inits)
           (indent 1) (printf "End~n~n"))
         
         (for-each (lambda (ev) (pp-Event ev port #:tab-level 1)) events)
          
         (unless (null? thms)
           (indent 1) (printf "Theorems~n")
           (for-each (lambda (thm) (indent 2) (printf "~a~n" thm)) thms))
         
         (unless (null? variants)
           (indent 1) (printf "Variants~n")
           (for-each (lambda (var) (indent 2) (printf "~a~n" car)) variants))
         
         (printf "End~n"))))))

(define typed-machine? 
  (match-lambda 
    ((struct Machine (name sees vars events invs variants thms inits))
     (and (andmap typed-context? sees)
          (andmap typed-event? events)
          (andmap (compose typed-predicate? Labelled-struct) invs)
          (andmap typed-expression? variants)
          (andmap (compose typed-predicate? Labelled-struct) thms)
          (andmap (compose typed-action? Labelled-struct) inits)))))

(provide/contract
 [struct Machine ((name string?) 
                  (sees (listof Context?))
                  (vars (or/c (listof Variable?) (listof typed-variable?)))
                  (events (listof Event?))
                  (invariants (listof labelled-predicate?))
                  (variants (listof expression?))
                  (theorems (listof labelled-predicate?))
                  (initialisation (listof labelled-action?)))]
 [typed-machine? (Machine? . -> . boolean?)]
 [machine-seen-constants (Machine? . -> . (listof (or/c Constant? typed-constant?)))]
 [machine-seen-axioms (Machine? . -> . (listof (or/c labelled-predicate? labelled-typed-predicate?)))]
 [machine-seen-sets (Machine? . -> . (listof Enumerated-Set?))])
