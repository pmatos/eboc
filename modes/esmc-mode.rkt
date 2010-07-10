#lang racket

;; Explicit State Model Checking mode
(require "../params.rkt"
         "../eventb-parser.rkt"
         "../pass-resolve-ids.rkt"
         "../pass-simplify.rkt"
         "../pass-posttyping-simplify.rkt"
         "../pass-typing.rkt"
         "../macro-utils.rkt"
         "../ast.rkt"
         "esmc-mode/generator.rkt")

(require (planet "modes/esmc-mode/search-sig.rkt" ("pjmatos" "eboc.plt" 1 0)))

(provide esmc-mode)

;; Options:
;; file - file to model check
;; bound - number of states to check
;; setbound - number of elements to add to deferred sets 
(define (esmc-mode mode-options)
  (let* ([requires (map (lambda (filename) 
                          `(require scheme/serialize 
                                    (planet ,(string-append "modes/esmc-mode/" filename) 
                                            ("pjmatos" "eboc.plt" 1 0))))
                        '("state.rkt" "serialize-utils.rkt" "eventb-lib.rkt" "scheduler.rkt" "search-sig.rkt" "value-generator.rkt" "probabilities.rkt"))]
         [file (cdr (assoc 'file mode-options))]
         [setbound (if (assoc 'setbound mode-options) (string->number (cdr (assoc 'setbound mode-options))) #f)]
         [ast (parameterize ([working-directory (build-path (working-directory) (path-only file))])
                (parse-machine file))]
         [interpret-ids-ast (time-apply-if "Interpret Identifiers" (verbose-mode)
                                           (lambda () (machine/resolve-ids ast)))]
         [simplified-ast (time-apply-if "Simplify Pass" (verbose-mode)
                                        (lambda ()
                                          (machine/simplify interpret-ids-ast 
                                                            #:defset-bound setbound
                                                            #:action-simplify 'det
                                                            #:init-simplify? #f
                                                            #:init-compact? #t)))]
         [typed-ast (time-apply-if "Typing Pass" (verbose-mode)
                                   machine/type simplified-ast)]
         [simp-typed-ast (time-apply-if "Post-Typing Simplify Pass" (verbose-mode)
                                        (lambda ()
                                          (typed-machine/simplify typed-ast)))]
         [gen-file (time-apply-if "Code Generation" (verbose-mode)
                                  generate-scheme-code typed-ast (Machine-invariants typed-ast) requires)]
         [search@ (dynamic-require gen-file 'search@)]
         [num-states (lambda () (string->number (cdr (assoc 'bound mode-options))))]
         [hash-states? (lambda () #t)]
         [debug? (lambda () (print-debug?))]
         [mc-result (time-apply-if "State Space Search" (verbose-mode)
                                   (lambda () 
                                     (when (debug?)
                                       (printf "Invoking search@ unit with parameters:~n")
                                       (printf "num-states: ~a~n" (num-states))
                                       (printf "hash-states?: ~a~n" (hash-states?))
                                       (printf "debug?: ~a~n" (debug?)))
                                     (invoke-unit search@ (import search^))))])
    (if (null? mc-result)
        (printf "No property was violated.~n")
        (for-each (match-lambda ((cons p trace)
                                 (printf "Violation ~a: ~a" p trace)))
                  mc-result))))