#lang racket

(require "environment.rkt"
         "ast.rkt")

;; This file implements a machine pass
;; that given a machine, returns the same machine, where all
;; identifier usage is resolved.
;; This can't be done during parsing due to PLT-Scheme lack of 
;; in-action parsing rules. As such, all identifiers during parsing are
;; set as identifiers and resolved by this pass to their proper usage.
;; So, no Identifier structures should be in the machine returned by this pass.

(provide (rename-out (pass-machine machine/resolve-ids)))

;; This pass keeps an environment, that checks if an identifier was defined as:
;; -  a set
;; -  a set literal (uninterpreted constant)
;; -  a variable
;; -  a constant
;;
;; so the environment maps symbols (which are identifier names) to another symbol,
;; representing their usage.

(define/contract (coerce-identifier id env)
  (symbol? Environment? . -> . (or/c Variable? Set? Set-Literal? Constant?))
  (let ([binding (env-get-binding env id)]) 
    (if binding
        (case (cdr binding)
          [(var) (make-Variable id)]
          [(set) (make-Set id)]
          [(set-literal) (make-Set-Literal id)]
          [(const) (make-Constant id)]
          [else (error 'coerce-identifier "Cannot match identifier type against know type. Binding is ~a" binding)]) 
        (error 'coerce-identifier "Cannot coerce identifier due to lack of information in environment. Given id: ~a, env: ~a" id env))))

(define (env-add/as-id env id-type)
  (match id-type
    ((struct Variable (name))
     (env-add-binding env name 'var))
    ((struct Set (name))
     (env-add-binding env name 'set))
    ((struct Set-Literal (name))
     (env-add-binding env name 'set-literal))
    ((struct Constant (name))
     (env-add-binding env name 'const))))

;; pass-machine : Machine -> Machine
(define (pass-machine ast (env (make-empty-env symbol? symbol=?))) ;; environment : symbol? -> (U 'var 'set 'set-literal 'const)
  (match ast
    ((struct Machine (name sees vars events invs variants theorems initialisation))
     (let* ([context/env-list (map (lambda (c) (pass-context c env)) sees)]
            [passed-contexts (map car context/env-list)]
            [machine-env (foldl (lambda (var acum-env)
                                  (env-add/as-id  acum-env var))
                                (if (null? context/env-list)
                                    env
                                    (apply env-merge (map cdr context/env-list)))
                                (append vars (map variable->post-variable vars)))])
       (make-Machine name 
                     passed-contexts 
                     vars
                     (map (lambda (e) (pass-event e machine-env)) events)
                     (map (lambda (i) 
                            (make-Labelled (Labelled-label i)
                                           (pass-predicate (Labelled-struct i) machine-env)))
                          invs)
                     (map (lambda (v) (pass-expression v machine-env)) variants)
                     (map (lambda (t) 
                            (make-Labelled (Labelled-label t)
                                           (pass-predicate (Labelled-struct t) machine-env))) 
                          theorems)
                     (map (lambda (init) 
                            (make-Labelled (Labelled-label init)
                                           (pass-action (Labelled-struct init) machine-env)))
                          initialisation))))))

;; pass-context : Context Environment -> (cons Context Environment)
(define (pass-context ctx env)
  (match ctx
    ((struct Context (name extensions sets constants axioms theorems))
     (let* ([context/env-pair (map (lambda (c) (pass-context c env)) extensions)]
            [contexts (map car context/env-pair)]
            [envs (map cdr context/env-pair)]
            [merged-envs (apply env-merge env envs)]
            [final-env
             (foldl (lambda (el acum-env) (env-add/as-id acum-env el))
                    merged-envs
                    (append constants 
                            sets
                            (append-map Enumerated-Set-vals (filter Enumerated-Set? sets))))])
       (cons 
        (make-Context name
                      contexts
                      sets
                      constants
                      (map (lambda (lpred) 
                             (make-Labelled (Labelled-label lpred)
                                            (pass-predicate (Labelled-struct lpred) final-env)))
                           axioms)
                      (map (lambda (lpred) 
                             (make-Labelled (Labelled-label lpred)
                                            (pass-predicate (Labelled-struct lpred) final-env)))
                           theorems))
        final-env)))))

;; pass-event : Event Environment -> Event
(define (pass-event evt env)
  (match evt
    ((struct Event (name status locals guards actions))
     ;; An event creates a new environment
     (let ([event-env (foldl (lambda (var env-acum) (env-add/as-id env-acum var))
                             env
                             locals)])
       (make-Event
        name
        status 
        locals
        (map (lambda (lpred) 
               (make-Labelled (Labelled-label lpred)
                              (pass-predicate (Labelled-struct lpred) event-env)))
             guards)
        (map (lambda (lact) 
               (make-Labelled (Labelled-label lact)
                              (pass-action (Labelled-struct lact) event-env))) actions))))))

;; pass-action : Action Environment -> Action
(define (pass-action act env)
  (match act
    ((struct Assign-Action (vars exprs))
     (make-Assign-Action (map (lambda (v) (pass-expression v env)) vars)
                         (map (lambda (e) (pass-expression e env)) exprs)))
    ((struct In-Assign-Action (var set-expr))
     (make-In-Assign-Action (pass-expression var env) (pass-expression set-expr env)))
    ((struct Suchthat-Assign-Action (vars pred))
     (make-Suchthat-Assign-Action (map (lambda (v) (pass-expression v env)) vars) (pass-predicate pred env)))))

;; pass-predicate : Predicate Environment -> Predicate
(define (pass-predicate pred env)
  (match pred
    ((struct Predicate-Literal _)
     pred)
    ((struct Quantifier (quant var body))
     (make-Quantifier quant var 
                      (pass-predicate body (env-add/as-id env var))))
    ((struct Predicate-UnOp (op arg))
     (make-Predicate-UnOp op (pass-predicate arg env)))
    ((struct Predicate-BinOp (op arg1 arg2))
     (make-Predicate-BinOp op (pass-predicate arg1 env) (pass-predicate arg2 env)))
    ((struct Predicate-RelOp (op arg1 arg2))
     (make-Predicate-RelOp op (pass-expression arg1 env) (pass-expression arg2 env)))
    ((struct Predicate-Partition (exprs))
     (make-Predicate-Partition (map (lambda (e) (pass-expression e env)) exprs)))))

;; pass-expression : Expression Environment -> Expression
(define (pass-expression expr env)
  (match expr
    ((struct Identifier (idname))
     (coerce-identifier idname env))
    ((or (struct Expression-Literal _)
         (struct Integer-Literal _)
         (struct Variable _)
         (struct Set-Literal _)
         (struct Set _)
         (struct Constant _))
     expr)
    ((struct Expression-Bool (pred))
     (make-Expression-Bool (pass-expression pred env)))
    ((struct Expression-UnOp (op arg))
     (make-Expression-UnOp op (pass-expression arg env)))
    ((struct Expression-BinOp (op arg1 arg2))
     (make-Expression-BinOp op (pass-expression arg1 env) (pass-expression arg2 env)))
    ((struct Lambda-Expression (idpat pred expr))
     (make-Lambda-Expression idpat (pass-expression pred env) (pass-expression expr env)))
    ((struct Set-Comprehension (compop vars pred expr))
     (let ([newenv (foldl (lambda (var newenv) (env-add/as-id newenv var)) env vars)])
       (make-Set-Comprehension compop vars (pass-expression pred newenv) (pass-expression expr newenv))))
    ((struct Set-Enumeration (exprs))
     (make-Set-Enumeration (map (lambda (e) (pass-expression e env)) exprs)))
    ((struct Variable-Pair (car cdr))
     (make-Variable-Pair (pass-expression car env) (pass-expression cdr env)))))
