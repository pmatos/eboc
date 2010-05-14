#lang scheme

(require "ast.scm"
         "types.scm")

;; Defines functions to work with environments which 
;; contain pairs Variables / Types

(define-struct Environment
  (bindings   ;; assoc list variables/types, note that an assoc list is preferred over the hash table given that this list is typically short.
   var-p?     ;; predicate for variables
   var-eq 
   )
  #:property prop:custom-write
  (lambda (struct port write?)
    (pp-env struct port)))

; Returns an empty environment
(define (make-empty-env var-p? var-eq)
  (make-Environment '() var-p? var-eq))

; Adds a binding to the environment and returns a new environment
; if a binding already exists for this variable proc is called with it.
(define (env-add-binding env var bind (proc (lambda (s) (error 'env-add-binding "Element already exists in environment: ~a" s))))
  (if (env-get-binding env var)
      (proc var)
      (make-Environment (cons (cons var bind) (Environment-bindings env))
                        (Environment-var-p? env)
                        (Environment-var-eq env))))

; Removes a binding from the environment
(define (env-del-binding env var) 
  (make-Environment (remove var 
                            (Environment-bindings env)
                            (Environment-var-eq env))
                    (Environment-var-p? env)
                    (Environment-var-eq env)))
                            
; Returns a type or #f if the variable is not in the list
(define (env-get-binding env var)
  (assf (lambda (x) ((Environment-var-eq env) x var))
        (Environment-bindings env)))

; Returns all the variables in this environment
(define (env-get-binding-ids env)
  (map car (Environment-bindings env)))
   
(define (extended-variable-name name post?)
  (if post?
      (string->symbol (string-append (symbol->string name) "'"))
      name))

; All envs should have the same procedures
(define (env-merge env1 . envs)
  (if (null? envs)
      env1
      (let ([lst (apply append (Environment-bindings env1) (map Environment-bindings envs))])
        (foldl (lambda (pair acum) 
                 (env-add-binding acum (car pair) (cdr pair) 
                                  (lambda (x)
                                    (error 'env-merge "Trying to redefine variable in environment: ~a~nenv: ~a~n" x acum))))
               (make-empty-env (Environment-var-p? env1) (Environment-var-eq env1))
               lst))))

(define (env-merge/check range-eq? env1 . envs)
  (if (null? envs)
      env1
      (let ([lst (apply append (Environment-bindings env1) (map Environment-bindings envs))])
        (foldl (lambda (pair acum) 
                 (env-add-binding acum (car pair) (cdr pair) 
                                  (lambda (x)
                                    (if (range-eq? (cdr pair)
                                                   (cdr (env-get-binding acum x)))
                                        acum
                                        (error 'env-merge "Trying to redefine variable with new value in environment: ~a~nenv: ~a~nnew value: ~a~n" x acum (cdr pair))))))
               (make-empty-env (Environment-var-p? env1) (Environment-var-eq env1))
               lst))))
  
  
;Pretty prints an environment
(define (pp-env env (port (current-output-port)))
  (fprintf port "Env: ")
  (for-each (lambda (binding)
              (fprintf port "(~a -> ~a) " (car binding) (cdr binding)))
            (Environment-bindings env)))


;                                                        
;                                                        
;     ;;;              ;                       ;         
;    ;   ;             ;                       ;         
;   ;      ;;;  ;;;; ;;;;;   ;;;; ;;;;   ;;;;;;;;;   ;;;;
;   ;     ;   ; ;   ;  ;     ;; ;     ; ;;     ;    ;    
;   ;     ;   ; ;   ;  ;     ;     ;;;; ;      ;    ;;;  
;   ;     ;   ; ;   ;  ;     ;    ;   ; ;      ;      ;;;
;    ;   ;;   ; ;   ;  ;     ;    ;  ;; ;;     ;        ;
;     ;;;  ;;;  ;   ;  ;;;   ;     ;;;;  ;;;;  ;;;  ;;;; 
;                                                        
;                                                        
;                                                        


(provide/contract
 [make-empty-env (->d ((var-p? contract?) (eq (var-p? var-p? . -> . boolean?))) 
                      ()
                      (e Environment?))]
 [env-get-binding (->d ((env Environment?) (var (Environment-var-p? env)))
                       ()
                       (res (or/c false/c (cons/c (Environment-var-p? env) any/c))))]
 [env-get-binding-ids (->d ((env Environment?)) 
                           ()
                           (vs (listof (Environment-var-p? env))))]
 [env-add-binding (->d ((env Environment?) (v (Environment-var-p? env)) (val any/c))
                       ((proc ((Environment-var-p? env) . -> . any/c)))
                       (res any/c))]
 [Environment? (any/c . -> . boolean?)]
 [env-merge ((Environment?) () #:rest (listof Environment?) . ->* . Environment?)] ;; TODO: Should we check that all the procs in envs are the same?
 [env-merge/check (((any/c any/c . -> . boolean?) Environment?) () #:rest (listof Environment?) . ->* . Environment?)] 
 [pp-env (Environment? . -> . void?)])