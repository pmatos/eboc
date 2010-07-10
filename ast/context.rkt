#lang racket

(require "labelled.rkt"
         "predexpr.rkt"
         "../params.rkt"
         "../untyped-utils.rkt"
         "../utils.rkt"
         "../types.rkt")

;                                                   
;                                                   
;                                                   
;     ;;;                  ;                    ;   
;    ;                     ;                    ;   
;   ;       ;;;   ;;;;   ;;;;;   ;;;   ;   ;  ;;;;; 
;   ;      ;   ;  ;;  ;    ;    ;;  ;   ; ;     ;   
;   ;      ;   ;  ;   ;    ;    ;   ;   ;;;     ;   
;   ;      ;   ;  ;   ;    ;    ;;;;;    ;      ;   
;   ;      ;   ;  ;   ;    ;    ;       ;;;     ;   
;    ;     ;   ;  ;   ;    ;    ;      ;; ;;    ;   
;     ;;;   ;;;   ;   ;    ;;;   ;;;;  ;   ;    ;;; 
;                                                   
;                                                   
;                                                   


(define-struct Context
  (name exts sets constants axioms theorems)
  #:property prop:custom-write
  (lambda (struct port write?)
    (pp-Context struct port)))

(define (make-empty-context)
  (make-Context "empty" '() '() '() '() '()))

(provide pp-Context)
(define (pp-Context struct (port (current-output-port)))
  (parameterize ([current-output-port port])
    (letrec ([indent (lambda (n) (printf (make-string (* (spaces/tab) n) #\space)))])
      (match struct
        ((struct Context (name exts sets consts axioms thms))
         (printf "Context ~a~n" name)
         
         (unless (null? exts)
           (indent 1) (printf "Extends~n")
           (for-each (lambda (cname) (indent 2) (printf "~a~n" cname)) (map Context-name exts)))
         
         (unless (null? sets)
           (indent 1) (printf "Sets~n")
           (for-each (lambda (set) 
                       (indent 2) 
                       (printf "~a~n" set))
                     sets))
         
         (unless (null? consts)
           (indent 1) (printf "Constants~n")
           (for-each (lambda (const) (indent 2) (printf "~a~n" const)) consts))
         
         (unless (null? axioms)
           (indent 1) (printf "Axioms~n")
           (for-each (lambda (ax) (indent 2) (printf "~a~n" ax))
                     axioms))
          
         (unless (null? thms)
           (indent 1) (printf "Theorems~n")
           (for-each (lambda (thm) (indent 2) (printf "~a~n" thm)) thms))
         
         (printf "End~n"))))))

(define typed-context?
  (match-lambda
    ((struct Context (_ exts _ _ axioms thms))
     (and (andmap typed-context? exts)
          (andmap (compose typed-predicate? Labelled-struct) axioms)
          (andmap (compose typed-predicate? Labelled-struct) thms)))))

;                                                                                                           
;                                                                                                           
;                                                                      ;                                    
;   ;;;;;                                              ;               ;          ;;;;           ;          
;   ;                                                  ;               ;         ;               ;          
;   ;      ;;;;   ;   ;  ;;;;;   ;;;    ; ;;;  ;;;   ;;;;;   ;;;    ;;;;         ;       ;;;   ;;;;;   ;;;  
;   ;      ;;  ;  ;   ;  ; ;  ; ;;  ;   ;;        ;    ;    ;;  ;  ;  ;;         ;;     ;;  ;    ;    ;;    
;   ;;;;;  ;   ;  ;   ;  ; ;  ; ;   ;   ;         ;    ;    ;   ;  ;   ;          ;;;   ;   ;    ;    ;;    
;   ;      ;   ;  ;   ;  ; ;  ; ;;;;;   ;      ;;;;    ;    ;;;;;  ;   ;             ;  ;;;;;    ;     ;;;  
;   ;      ;   ;  ;   ;  ; ;  ; ;       ;     ;   ;    ;    ;      ;   ;             ;  ;        ;        ; 
;   ;      ;   ;  ;   ;  ; ;  ; ;       ;     ;   ;    ;    ;      ;   ;             ;  ;        ;        ; 
;   ;;;;;  ;   ;   ;;;;  ; ;  ;  ;;;;   ;     ;;;;;    ;;;   ;;;;   ;;;;         ;;;;    ;;;;    ;;;  ;;;;  
;                                                                                                           
;                                                                                                           
;                                                                                                           


(define-struct (Enumerated-Set Set) ;; Inherits name from set
  (vals)
  #:property prop:custom-write
  (lambda (struct port write?)
    (pp-Enumerated-Set struct port)))

(define (pp-Enumerated-Set struct (port (current-output-port)))
  (fprintf port "~a = {" (Set-name struct))
  (let loop ([vals (Enumerated-Set-vals struct)])
    (if (null? (rest vals))
        (fprintf port "~a}\n" (first vals))
        (begin
          (fprintf port "~a, " (first vals))
          (loop (rest vals))))))

(define Enumerated-Set-vals/c
  (flat-named-contract 'Enumerated-Set-vals
                       (lambda (vals)
                         (and/c (not/c (null? vals)) (listof Set-Literal?)))))


;                                     
;                                     
;                   ;    ;;;          
;   ;   ;    ;             ;          
;   ;   ;    ;             ;          
;   ;   ;  ;;;;;   ;;      ;     ;;;  
;   ;   ;    ;      ;      ;    ;;    
;   ;   ;    ;      ;      ;    ;;    
;   ;   ;    ;      ;      ;     ;;;  
;   ;   ;    ;      ;      ;        ; 
;   ;   ;    ;      ;      ;        ; 
;    ;;;     ;;;  ;;;;;    ;;;  ;;;;  
;                                     
;                                     
;              

(define (context-seen-constants ctx)
  (append (Context-constants ctx)
          (append-map context-seen-constants (Context-exts ctx))))

(define (context-seen-axioms ctx)
  (append (Context-axioms ctx)
          (append-map context-seen-axioms (Context-exts ctx))))

(define (context-seen-sets ctx)
  (append (Context-sets ctx)
          (append-map context-seen-sets (Context-exts ctx))))

(define (context-merge ctx1 . ctxs)
  (letrec ([flatten-context 
            (lambda (ctx) ;; Returns a list of contexts that is ctx itself the parents obtained recursively from ctx
              (if (null? (Context-exts ctx))
                  (list ctx)
                  (cons ctx (append-map flatten-context (Context-exts ctx)))))])
    (foldl (lambda (ctx ctx-accum)
             (make-Context (string-append "(" (Context-name ctx-accum) "_m_" (Context-name ctx) ")")
                           '()
                           (append (Context-sets ctx-accum) (Context-sets ctx))
                           (append (Context-constants ctx-accum) (Context-constants ctx))
                           (append (Context-axioms ctx-accum) (Context-axioms ctx))
                           (append (Context-theorems ctx-accum) (Context-theorems ctx))))
           ctx1
           (append (append-map flatten-context ctxs) (rest (flatten-context ctx1))))))

(define (bound-deferred-sets ctx bound)
  (match ctx
    ((struct Context (name exts sets consts axioms thms))
     (make-Context name 
                   exts
                   (map (lambda (s)
                          (if (Enumerated-Set? s)
                              s
                              (make-Enumerated-Set (Set-name s) (build-list bound (lambda (n) (make-Set-Literal (gensym (symbol-append (Set-name s) ':))))))))
                        sets)
                   consts
                   axioms
                   thms))))

;; Given a context returns an assoc with set name -> set of enumerated sets in the context
;; and seen contexts
(define (get-enumerated-sets/assoc context)
  (apply append 
         (map (lambda (s) (cons (Set-name s) s)) (filter Enumerated-Set? (Context-sets context)))
         (map get-enumerated-sets/assoc (Context-exts context))))

(define (get-enumerated-sets-as-types/assoc context)
  (map (lambda (p) (cons (car p) (enumerated-set->type-enumeration (cdr p))))
       (get-enumerated-sets/assoc context)))

(define (get-enumerated-sets-as-set-enumeration/assoc context)
  (map (lambda (p) (enumerated-set->set-enumeration/cons (cdr p)))
       (get-enumerated-sets/assoc context)))

(define (enumerated-set->type-enumeration enumset)
  (make-Type-Enumeration (Set-name enumset)
                         (map Set-Literal-name (Enumerated-Set-vals enumset))))

(define (enumerated-set->set-enumeration/cons enumset)
  (cons (make-Set (Set-name enumset))
        (make-Set-Enumeration (Enumerated-Set-vals enumset))))


;                                                                 
;                                                                 
;                                                                 
;     ;;;                  ;                           ;          
;    ;                     ;                           ;          
;   ;       ;;;   ;;;;   ;;;;;   ; ;;;  ;;;     ;;;  ;;;;;   ;;;  
;   ;      ;   ;  ;;  ;    ;     ;;        ;   ;       ;    ;;    
;   ;      ;   ;  ;   ;    ;     ;         ;  ;        ;    ;;    
;   ;      ;   ;  ;   ;    ;     ;      ;;;;  ;        ;     ;;;  
;   ;      ;   ;  ;   ;    ;     ;     ;   ;  ;        ;        ; 
;    ;     ;   ;  ;   ;    ;     ;     ;   ;   ;       ;        ; 
;     ;;;   ;;;   ;   ;    ;;;   ;     ;;;;;    ;;;    ;;;  ;;;;  
;                                                                 
;                                                                 
;                                                                 

(provide/contract
 [struct Context ((name string?)
                  (exts (listof Context?))
                  (sets (listof Set?))
                  (constants (or/c (listof Constant?) (listof typed-constant?)))
                  (axioms (listof labelled-predicate?))
                  (theorems (listof labelled-predicate?)))]
 [make-empty-context (-> Context?)] 
 [context-merge ((Context?) (Context?) . ->* . Context?)]
 [bound-deferred-sets (Context? natural-number/c . -> . Context?)]
 [get-enumerated-sets/assoc (Context? . -> . (listof (cons/c symbol? Enumerated-Set?)))]
 [get-enumerated-sets-as-types/assoc (Context? . -> . (listof (cons/c symbol? Type-Enumeration?)))]
 [get-enumerated-sets-as-set-enumeration/assoc (Context? . -> . (listof (cons/c Set? Set-Enumeration?)))]
 [enumerated-set->set-enumeration/cons (Enumerated-Set? . -> . (cons/c Set? Set-Enumeration?))]
 [enumerated-set->type-enumeration (Enumerated-Set?  . -> . Type-Enumeration?)]
 [struct (Enumerated-Set Set) ((name symbol?) (vals Enumerated-Set-vals/c))]
 [context-seen-constants (Context? . -> . (listof (or/c Constant? typed-constant?)))]
 [context-seen-axioms (Context? . -> . (listof (or/c labelled-predicate? labelled-typed-predicate?)))]
 [context-seen-sets (Context? . -> . (listof Enumerated-Set?))]
 [typed-context? (Context? . -> . boolean?)])