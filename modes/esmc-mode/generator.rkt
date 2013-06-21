#lang racket/base
;;
;;    This file is part of Eboc (Event-B Model Checker).
;;
;;    Eboc is free software: you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation, either version 3 of the License, or
;;    (at your option) any later version.
;;
;;    Eboc is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.
;;
;;    You should have received a copy of the GNU General Public License
;;    along with Eboc.  If not, see <http://www.gnu.org/licenses/>.
;;

(require racket/match
         racket/file
         racket/list
         (rename-in (only-in racket/pretty pretty-write)
                    (pretty-write pretty-print))
         racket/serialize
         "../../ast.rkt"
         "../../utils.rkt")

(provide generate-scheme-code)

#| 
This module given an eventb ast,
generates code for explicit state model check as follows:

For the AST and a list of properties a single file of procedures is generated. This file is linked to the event-b
library and search procedures for the final executable file.
The procedures generated are:
- One predicate <event-name>-guard/solver per event
  Receives 2 arguments: current state and list of locals
  Returns an assignment to the local variables (assoc) that satisfy the guard or #f if none exists. 
  Note that if it is called again, it will generate always a different value combination for the local variables that satisfy the guard.
  
  This is in fact, not a simple predicate. It is an enumerator of values that satisfy a given predicate for a set of values and it might take
  some time to compute.

- One procedure <event-name>-transition per event
  Receives 2 arguments: the current state and an assignment to the local variables (assoc).
  Returns a new state.

- One predicate procedure <property-name>? per property to be checked.
  Receives 1 argument: the current state
  Returns 1 argument: a boolean, if the property is true in current state or not
|#

;; Receives an AST and an assoc list of Labelled-Predicate).
;; Returns the file name where the scheme code was written to.
(define (generate-scheme-code mch-ast props require-sexprs)
  (let ([filename (make-temporary-file)])
    (printf "Generating code to: ~a~n" filename)
    (call-with-output-file filename 
      #:mode 'text 
      #:exists 'replace
      (lambda (fp)
        (generate-header-code fp require-sexprs)
        (fprintf fp "(define-unit search@ (import search^) (export)~n")
        (generate-events-code fp (Machine-events mch-ast))
        (generate-props-code fp props)
        (generate-search-init-code fp 
                                   (Machine-initialisation mch-ast)
                                   (map Event-name (Machine-events mch-ast))
                                   (map Labelled-label props)
                                   (machine-seen-sets mch-ast)
                                   (machine-seen-constants mch-ast)
                                   (map Labelled-struct (machine-seen-axioms mch-ast)))
        (fprintf fp ")"))); closes define-unit
    filename))

(define (generate-header-code fp require-sexprs)
  (fprintf fp "#lang racket/base~n~n")
  (fprintf fp ";; Code generated automatically. Not supposed to be edited.~n")
  (for-each (lambda (require) (write require fp)) require-sexprs)
  (write '(provide search@) fp))

(define (generate-props-code fp props)
  (fprintf fp ";                                     ~n")
  (fprintf fp ";                                     ~n")
  (fprintf fp ";                                     ~n")
  (fprintf fp ";   ;;;;;                             ~n")
  (fprintf fp ";   ;    ;                            ~n")
  (fprintf fp ";   ;    ;  ; ;;   ;;;   ;;;;    ;;;  ~n")
  (fprintf fp ";   ;    ;  ;;  ; ;; ;;  ;; ;;  ;   ; ~n")
  (fprintf fp ";   ;;;;;   ;     ;   ;  ;   ;  ;     ~n")
  (fprintf fp ";   ;       ;     ;   ;  ;   ;   ;;;  ~n")
  (fprintf fp ";   ;       ;     ;   ;  ;   ;      ; ~n")
  (fprintf fp ";   ;       ;     ;; ;;  ;;  ;  ;   ; ~n")
  (fprintf fp ";   ;       ;      ;;;   ;;;;    ;;;  ~n")
  (fprintf fp ";                        ;            ~n")
  (fprintf fp ";                        ;            ~n")
  (fprintf fp ";                        ;            ~n")
  (for-each (match-lambda ((struct Labelled (name prop)) (generate-property-code fp name prop))) props))
     
(define (generate-events-code fp events)
  (for-each (lambda (ev)
              (if (null? (Event-locals ev))
                  (generate-guard-proc/det fp (Event-name ev) (Labelled-struct (first (Event-guards ev))))
                  (generate-guard-proc/nondet fp (Event-name ev) (Labelled-struct (first (Event-guards ev))) (Event-locals ev)))
              (generate-action-proc fp (Event-name ev) (Labelled-struct (first (Event-actions ev)))))
            events))

(define (event-guard-proc-name name)
  (symbol-append name '-guard))

(define (event-action-proc-name name)
  (symbol-append name '-action))

(define (prop-proc-name name)
  (symbol-append name '-prop?))

(define (generate-guard-proc/det fp name guard)
  (let ([guard-name (event-guard-proc-name name)]
        [action-name (event-action-proc-name name)])
    (pretty-print `(define (,guard-name state)
                     ;(printf "~nGenerating deterministic guard for ~a with state ~a~n." ',guard-name state)
                     (let ([enabled? (with-handlers 
                                         (((lambda (v) 
                                             (or (eq? v 'fail-funimage)
                                                 (eq? v 'fail-upto))) 
                                           (lambda (v) #f)))
                                       (eval-predicate (deserialize 
                                                        ',(serialize (strip-types guard))) 
                                                       state))]
                           [done? #f])
                       (lambda (msg)
                         ;(printf "~n~nPassing msg ~a to guard ~a in state ~a.~n" msg ',guard-name state)
                         (case msg
                           [(empty?) (or done? (not enabled?))] ;; This is empty? if it's done or not enabled.
                           [(pre-stt) state]
                           [(ev-name) ',name]
                           [(stt) (set! done? #t) (,action-name state '())]
                           [(prt) 1.0]
                           [else (error ',guard-name "Unexpected message to deterministic action generator ~a" msg)]))))
                  fp)))

(define (generate-guard-proc/nondet fp name guard locals (consts '()) (axioms '()) (sets '()))
  (let ([guard-name (event-guard-proc-name name)]
        [action-name (event-action-proc-name name)])
    (pretty-print `(define (,guard-name state)
                     ;(printf "~nGenerating non-deterministic guard ~a with state ~a~n." ',guard-name state)
                     (let* ([enum (type-list-enumerator 
                                   (map deserialize ',(append (map serialize (map Expr/wt-type locals))
                                                              (map serialize (map Expr/wt-type consts)))))]
                            [next-enum (enum)]
                            [next-prt (enum 'prt)])
                       (lambda (msg)
                         ;(printf "~n~nPassing msg ~a to guard ~a in state ~a.~n" msg ',guard-name state)
                         (case msg
                           [(empty?) (not next-enum)] ;; if next-enum is #f
                           [(pre-stt) state]
                           [(ev-name) ',name]
                           [(stt) ;; may return #f in which case there is no state from this try
                            (let ([local-state (map cons 
                                                    (map deserialize
                                                         ',(map (compose serialize Expr/wt-expr) 
                                                                (append locals consts)))
                                                    (to-eb-values next-enum))])
                              ;(printf "Returning next enumeration ~a for guard ~a with local state ~a.~n" next-enum ',guard-name local-state)
                              (begin0
                                (if (with-handlers 
                                         (((lambda (v) 
                                             (or (eq? v 'fail-funimage)
                                                 (eq? v 'fail-upto))) 
                                           (lambda (v) #f)))
                                      (eval-predicate (deserialize 
                                                       ',(serialize 
                                                          (strip-types
                                                           (if (null? axioms)
                                                               guard
                                                               (make-Predicate-BinOp 'land 
                                                                                     guard 
                                                                                     (foldl (lambda (a acum) (make-Predicate-BinOp 'land acum a))
                                                                                            (first axioms)
                                                                                            (rest axioms)))))))
                                                    state local-state))
                                    (,action-name state local-state)
                                    #f)
                                (set! next-enum (enum))
                                (set! next-prt (enum 'prt))))]
                           [(prt) next-prt]
                           [else (error ',guard-name "Unexpected message to non-deterministic action generator ~a" msg)]))))
                  fp)))
                 
;; Procedure that generates code for a procedure that given
;; the current state and the local state computes the next state.
;; The action that defines the state change is passed in as an action ast node.
(define (generate-action-proc fp name action (init? #f) (consts '()))
  (let ([action-name (event-action-proc-name name)])
    (pretty-print `(define (,action-name state local-state)
                     (let ([newstate (foldl (lambda (assign-pair acum)
                                              ,(if init?
                                                   '(state-set acum (car assign-pair) (eval-expression (cdr assign-pair) state local-state))
                                                   '(state-update acum 
                                                                 (car assign-pair)  
                                                                 (eval-expression (cdr assign-pair) state local-state))))
                                            state
                                            (deserialize-assoc
                                             ',(let ([actions (map cons 
                                                                   (append (map (compose serialize strip-types) (Assign-Action-lhs action))
                                                                           (map (compose serialize strip-types) consts))
                                                                   (append (map (compose serialize strip-types) (Assign-Action-rhs action))
                                                                           (map (compose serialize strip-types) consts)))])
                                                 ;(printf "Generated actions for ~a:~nAction: ~a~nExpr: ~a~n" name action actions) 
                                                 actions)))])
                       ;(printf "Execution action ~a with state ~a and local state ~a resulting in state ~a.~n" ',action-name state local-state newstate)
                       newstate))
                  fp))) 

(define (generate-property-code fp name prop)
  (let ([prop-name (prop-proc-name name)])
    (pretty-print `(define (,prop-name state)
                     ;(printf "Checking property ~a on state ~a.~n" ',prop-name state)
                     (with-handlers (((lambda (v) 
                                        (or (eq? v 'fail-funimage)
                                            (eq? v 'fail-upto))) 
                                      (lambda (v) #f)))
                       (eval-predicate (deserialize ',(serialize (strip-types prop))) state)))
                  fp)))

;; inits should be a list of at most 2 elements: an Assign-Action and Suchthat-Assign-Action
(define (generate-search-init-code fp inits event-names prop-names esets consts axioms)
  (match (map Labelled-struct inits)
    ((list (struct Assign-Action _))
     (if (null? consts)
         (generate-guard-proc/det fp '*Initialisation* ebtrue)
         (generate-guard-proc/nondet fp '*Initialisation* ebtrue '() consts axioms))
     (generate-action-proc fp '*Initialisation* (Labelled-struct (first inits)) #t consts))
    ((list (struct Suchthat-Assign-Action (vars pred)))
     (generate-guard-proc/nondet fp '*Initialisation* pred vars consts axioms)
     (generate-action-proc fp '*Initialisation* (make-Assign-Action vars vars) #t consts))
    ((list-no-order (struct Assign-Action (vars/det exprs)) (struct Suchthat-Assign-Action (vars/ndet pred)))
     (generate-guard-proc/nondet fp '*Initialisation* pred vars/ndet consts axioms)
     (generate-action-proc fp '*Initialisation* (make-Assign-Action (append vars/det vars/ndet) (append exprs vars/ndet)) #t consts)))
  ;; The following line registers the initialisation. It passes the very first state.
  ;; This initial state starts with the definition for the enumerated sets available.
  ;; If and when we find a way to deal with sets without enumerating them then this might have to
  ;; be changed.
  (pretty-print 
   `(register-initialisation! (,(event-guard-proc-name '*Initialisation*) 
                               (deserialize-assoc
                                ',(map (lambda (eset)
                                         (let ([set/cons (enumerated-set->set-enumeration/cons eset)])
                                           (cons (serialize (car set/cons))
                                                 (serialize (cdr set/cons)))))
                                       esets))))
   fp)

  ;; Code to register events procedures
  (for-each (lambda (name) 
              (pretty-print `(register-event! ',name ,(event-guard-proc-name name) ,(event-action-proc-name name))
                            fp))
            event-names)
  
  ;; Code to register property procedures
  (for-each (lambda (name)
              (pretty-print `(register-property! #:name ',name ,(prop-proc-name name)) fp))
            prop-names)
  
  ;; Start search
  (pretty-print '(search! (num-states) #:debug? (debug?) #:cache-states? (hash-states?)) fp))
                                                      
                           