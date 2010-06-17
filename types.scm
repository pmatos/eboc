#lang scheme/base

(require scheme/match
         scheme/serialize
         scheme/list
         scheme/contract
         (only-in srfi/1 every cons*)
         "params.scm"
         "utils.scm"
         "printer-tables.scm")

(provide pp-type
         t
         finite-type?
         type?
         type=?
         ground-type?
         type->ground-type
         undef-type->type
         unify-types/uniquely
         unify-types
         make-fresh-type-variable
         make-fresh-type-variable-name
         (struct-out Type-Powerset)
         (struct-out Type-CartesianProduct)
         (struct-out Type-Integer)
         (struct-out Type-Boolean)
         (struct-out Type-Polymorphic)
         (struct-out Type-UndefEnumeration))


;; Event-B types
;; This module contains definitions for creating Event-B types.

(define-serializable-struct Type-Powerset
  (subtype)
  #:guard 
  (lambda (subtype type-name)
    (values (if (type? subtype)
                subtype
                (error 'Type-Powerset:guard "Guard failed."))))
  #:property prop:custom-write
  (lambda (struct port write?)
    (pp-type struct port)))

(define-serializable-struct Type-CartesianProduct
  (subtype1 
   subtype2)
  #:guard
  (lambda (subtype1 subtype2 type-name)
    (unless (and (type? subtype1) (type? subtype2))
      (error 'Type-CartesianProduct:guard "Failed with subtypes: ~a, ~a" subtype1 subtype2))
    (values subtype1 subtype2))
  #:property prop:custom-write
  (lambda (struct port write?)
    (pp-type struct port)))

(define-serializable-struct  Type-Integer
  ()
  #:property prop:custom-write
  (lambda (struct port write?)
    (pp-type struct port)))

(define-serializable-struct Type-Boolean
  ()
  #:property prop:custom-write
  (lambda (struct port write?)
    (pp-type struct port)))

(define-serializable-struct Type-Polymorphic
  (name)
  #:guard
  (lambda (name type-name)
    (unless (symbol? name)
      (error 'Type-Polymorphic "Guard failed, given ~a" name))
    values name)
  #:property prop:custom-write
  (lambda (struct port write?)
    (pp-type struct port)))

(define (make-fresh-type-variable)
  (make-Type-Polymorphic (make-fresh-type-variable-name)))

(define (make-fresh-type-variable-name)
  (gensym 'typevar:))

(define-serializable-struct Type-UndefEnumeration
  (name)
  #:guard
  (lambda (name type-name)
    (unless (symbol? name)
      (error 'Type-UndefEnumeration:guard "Guard failed, got ~a" name))
    (values name))
  #:property prop:custom-write
  (lambda (struct port write?)
    (pp-type struct port)))

(define-serializable-struct (Type-Enumeration Type-UndefEnumeration)
  (enum)
  #:property prop:custom-write
  (lambda (struct port write?)
    (pp-type struct port)))


;; Pretty Printing
(define (pp-type type (port (current-output-port)))
  (when print-debug?
    (fprintf port "Type["))
  (match type
    ((struct Type-Integer ())
     (print-table-constant type-op-table 'Z port))
    ((struct Type-Boolean ())
     (print-table-constant type-op-table 'B port))
    ((struct Type-Polymorphic (name))
     (fprintf port "~a" name))
    ((struct Type-Enumeration (name enum))
     (fprintf port "~a:{" name)
     (for-each (lambda (s)
                 (fprintf port "~a," s))
               enum)
     (fprintf port "}"))
    ((struct Type-UndefEnumeration (name))
     (fprintf port "s:~a" name))
    ((struct Type-Powerset (subtype))
     (print-table-constant type-op-table 'P port)
     (fprintf port "(")
     (pp-type subtype port)
     (fprintf port ")"))
    ((struct Type-CartesianProduct (s1 s2))
     (fprintf port "(")
     (pp-type s1 port)
     (fprintf port " ")
     (print-table-constant type-op-table 'x port)
     (fprintf port " ")
     (pp-type s2 port)
     (fprintf port ")")))
  (when print-debug?
    (fprintf port "]")))

;; Testing for types
(define (type? u)
  (anyof? (list Type-Powerset? Type-CartesianProduct? Type-Integer? Type-Boolean? Type-UndefEnumeration? Type-Enumeration? Type-Polymorphic?)
          u))

(define (ground-type? u)
  (match u
    ((or (struct Type-Integer _)
         (struct Type-Boolean _)
         (struct Type-UndefEnumeration _)
         (struct Type-Enumeration _))
     #t)
    ((struct Type-Powerset (subtype))
     (ground-type? subtype))
    ((struct Type-CartesianProduct (subtype1 subtype2))
     (and (ground-type? subtype1) (ground-type? subtype2)))
    (_ #f)))

(define (finite-type? u)
  (match u
    ((or (struct Type-Enumeration _)
         (struct Type-Boolean _))
     #t)
    ((or (struct Type-Integer _)
         (struct Type-UndefEnumeration _))
     #f)
    ((struct Type-Powerset (subtype))
     (finite-type? subtype))
    ((struct Type-CartesianProduct (subtype1 subtype2))
     (and (finite-type? subtype1) (finite-type? subtype2)))
    (_ #f)))

(define t
  (match-lambda 
    ['Z (make-Type-Integer)]
    ['B (make-Type-Boolean)]
    [`(P ,a) (make-Type-Powerset (t a))]
    [`(,a x ,b) (make-Type-CartesianProduct (t a) (t b))]
    [`(,name : (,els ..1)) (make-Type-Enumeration name els)]
    [(? symbol? a) (make-Type-Polymorphic a)]
    [(? type? a) a]))

;; Given a type and a substitution that provides an association between type variables and 
;; ground types, returns type but as a ground type, i.e., without type variables.
;; Obviously, given a ground type, returns a ground type.
(define (type->ground-type type subst)
  (match type
    ((or (struct Type-Integer _)
         (struct Type-Boolean _)
         (struct Type-Enumeration _)
         (struct Type-UndefEnumeration _))
     type)
    ((struct Type-Powerset (subtype))
     (make-Type-Powerset (type->ground-type subtype subst)))
    ((struct Type-CartesianProduct (subtype1 subtype2))
     (make-Type-CartesianProduct (type->ground-type subtype1 subst)
                                 (type->ground-type subtype2 subst)))
    ((struct Type-Polymorphic (name))
     (let ([ground-type (assf (lambda (var) 
                                (eqv? name (Type-Polymorphic-name var)))
                              subst)])
       (if ground-type
           (cdr ground-type)
           (error 'type->ground-type "Unable to generate ground type, no substitution for type variable.~nType Variable: ~a~nSubstitution: ~a~n" type subst))))))

;; Given a type that might contain Undefined enumerations
;; and a set list, returns a new type without undefined enumerations
;; by replace each undefined enumeration by its defined counterpart based
;; on the sets defined in set-list
(define (undef-type->type type enumset-list/assoc)
  (match type
    ((or (struct Type-Integer _)
         (struct Type-Boolean _)
         (struct Type-Polymorphic _)
         (struct Type-Enumeration _))
     type)
    ((struct Type-Powerset (subtype))
     (make-Type-Powerset (undef-type->type subtype enumset-list/assoc)))
    ((struct Type-CartesianProduct (subtype1 subtype2))
     (make-Type-CartesianProduct (undef-type->type subtype1 enumset-list/assoc)
                                 (undef-type->type subtype2 enumset-list/assoc)))
    ((struct Type-UndefEnumeration (name))
     (let ([el (assq name enumset-list/assoc)])
       (if el
           (cdr el)
           (error 'undef-type->type 
                  "Cannot remove undefined type ~a, given the list of sets ~a" type (map cdr enumset-list/assoc)))))))

;; Type Equality
(define (type=? t1 t2)
  (or (and (Type-Polymorphic? t1)
           (Type-Polymorphic? t2)
           (eqv? (Type-Polymorphic-name t1) (Type-Polymorphic-name t1)))
      (and (Type-Integer? t1) (Type-Integer? t2))
      (and (Type-Boolean? t1) (Type-Boolean? t2))
      (and (Type-Enumeration? t1) (Type-Enumeration? t2) 
           (equal? (Type-Enumeration-enum t1) (Type-Enumeration-enum t2)))
      (and (Type-Powerset? t1) (Type-Powerset? t2)
           (type=? (Type-Powerset-subtype t1) (Type-Powerset-subtype t2)))
      (and (Type-CartesianProduct? t1) (Type-CartesianProduct? t2)
           (type=? (Type-CartesianProduct-subtype1 t1) (Type-CartesianProduct-subtype1 t2))
           (type=? (Type-CartesianProduct-subtype2 t1) (Type-CartesianProduct-subtype2 t2)))))

(define (unify-types/uniquely type-system)
  (let ([subst (unify-types type-system)])
    (if (and subst (every (lambda (p) (ground-type? (cdr p))) subst))
        subst
        (begin0 
          #f
          (when (and subst (print-debug?))
            (printf "Warning: Failed to solve type system uniquely.~nSubstitution found: ~a~n" subst))))))

;; Type Unification
;; Given a list of type pairs, returns a list of substitutions for each type variable
;; or #f if none exist that substitute uniquely.
(define (unify-types type-system) ;; type-system is a list of pairs of types to unify uniquely
  (letrec ([replace-in-type
            (lambda (x y t) ;; Replace x by y in type t, x is always a polymorphic variable
              (match t
                ((struct Type-Polymorphic ((? (lambda (name) (eqv? (Type-Polymorphic-name x) name)))))
                 y)
                ((struct Type-Powerset (subtype))
                 (make-Type-Powerset (replace-in-type x y subtype)))
                ((struct Type-CartesianProduct (subtype1 subtype2))
                 (make-Type-CartesianProduct 
                  (replace-in-type x y subtype1)
                  (replace-in-type x y subtype2)))
                (_ t)))]
           [replace-in-constraints 
            (lambda (x y lst) ;; replaces x by y in the list of pairs of types
              (map (lambda (p)
                     (cons (replace-in-type x y (car p))
                           (replace-in-type x y (cdr p))))
                   lst))]
           [occurs-check?
            (lambda (x y) ;; verifies if x occurs in y
              (match y
                ((struct Type-Polymorphic ((? (lambda (name) (eqv? (Type-Polymorphic-name x) name)))))
                 #t)
                ((struct Type-Powerset (subtype))
                 (occurs-check? x subtype))
                ((struct Type-CartesianProduct (subtype1 subtype2))
                 (or (occurs-check? x subtype1)
                     (occurs-check? x subtype2)))
                (_ #f)))])
    
    ;; Lets add them all to a stack
    (let loop ([stack type-system] [subst '()])
      (if (null? stack)
          subst
          (let* ([top (first stack)]
                 [lhs (car top)]
                 [rhs (cdr top)])
            (cond [(or (and (Type-Integer? lhs)
                            (Type-Integer? rhs))
                       (and (Type-Boolean? lhs)
                            (Type-Boolean? rhs))
                       (and (Type-Enumeration? lhs)
                            (Type-Enumeration? rhs)
                            (eqv? (Type-UndefEnumeration-name lhs) (Type-UndefEnumeration-name rhs))))
                   (loop (rest stack) subst)]
                  [(Type-Polymorphic? lhs)
                   (if (occurs-check? lhs rhs)
                       (begin0
                         #f
                         (when (print-debug?)
                           (printf "Warning: Failed to solve type system because occurs-check failed.~nCurrent stack: ~a~nCurrent Substitution: ~a~n" stack subst)))
                       (loop (replace-in-constraints lhs rhs (rest stack))
                             (cons (cons lhs rhs) (replace-in-constraints lhs rhs subst))))]
                  [(Type-Polymorphic? rhs)
                   (if (occurs-check? rhs lhs)
                       (begin0
                         #f
                         (when (print-debug?)
                           (printf "Warning: Failed to solve type system because occurs-check failed.~nCurrent stack: ~a~nCurrent Substitution: ~a~n" stack subst)))
                       (loop (replace-in-constraints rhs lhs (rest stack))
                             (cons (cons rhs lhs) (replace-in-constraints rhs lhs subst))))]
                  [(and (Type-Powerset? lhs)
                        (Type-Powerset? rhs))
                   (loop (cons (cons (Type-Powerset-subtype lhs) (Type-Powerset-subtype rhs)) (rest stack)) subst)]
                  [(and (Type-CartesianProduct? lhs)
                        (Type-CartesianProduct? rhs))
                   (loop (cons* (cons (Type-CartesianProduct-subtype1 lhs) (Type-CartesianProduct-subtype1 rhs))
                                (cons (Type-CartesianProduct-subtype2 lhs) (Type-CartesianProduct-subtype2 rhs))
                                (rest stack))
                         subst)]
                  [else 
                   (when (print-debug?)
                     (printf "Warning: Failed to solve type system there is no matching condition for the unification to proceed.~nCurrent stack: ~a~nCurrent Substitution: ~a~n" stack subst))
                   #f]))))))



;                                                                        
;                                                                        
;                                                                        
;     ;;;                         ;                           ;          
;    ;   ;                        ;                           ;          
;   ;       ;;;   ; ;;    ;;;   ;;;;;   ; ;;   ;;;    ;;;   ;;;;;   ;;;  
;   ;      ;; ;;  ;;  ;  ;   ;    ;     ;;  ; ;   ;  ;;  ;    ;    ;   ; 
;   ;      ;   ;  ;   ;  ;        ;     ;         ;  ;        ;    ;     
;   ;      ;   ;  ;   ;   ;;;     ;     ;      ;;;;  ;        ;     ;;;  
;   ;      ;   ;  ;   ;      ;    ;     ;     ;   ;  ;        ;        ; 
;    ;   ; ;; ;;  ;   ;  ;   ;    ;     ;     ;   ;  ;;  ;    ;    ;   ; 
;     ;;;   ;;;   ;   ;   ;;;     ;;;   ;      ;; ;   ;;;     ;;;   ;;;  
;                                                                        
;                                                                        
;                                                                    ;; ;

(provide/contract
 [struct Type-Enumeration ((name symbol?) (enum (listof symbol?)))])