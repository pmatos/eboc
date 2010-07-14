#lang racket

(require (only-in srfi/1 every)
         "predexpr.rkt"
         "../utils.rkt")

(provide action?)

(define (action? u)
  (anyof? (list Assign-Action? In-Assign-Action? Suchthat-Assign-Action?)
          u))

(define-struct Assign-Action
  (lhs rhs)
  #:guard 
  (lambda (lhs rhs type-name)
    (if (= (length lhs) (length rhs))
        (values lhs rhs)
        (error 'Assign-Action:guard 
               "Failed creation of Assign-Action because lhs and rhs don't have the same length.\nlhs: ~a, rhs: ~a\n" lhs rhs)))
  #:property prop:custom-write 
  (lambda (struct port write?)
    (pp-Assign-Action struct port)))

(define (pp-Assign-Action struct (port (current-output-port)))
  ;; Print lhs
  (let loop ([rest-lhs (Assign-Action-lhs struct)])
    (if (null? (rest rest-lhs))
        (fprintf port "~a" (first rest-lhs))
        (begin 
          (fprintf port "~a, " (first rest-lhs))
          (loop (rest rest-lhs)))))
  
  (fprintf port " := ")
  
  ;; Print rhs
  (let loop ([rest-rhs (Assign-Action-rhs struct)])
    (pp-expression (first rest-rhs) port)
    (unless (null? (rest rest-rhs))
      (fprintf port ", ")
      (loop (rest rest-rhs)))))

(provide (struct-out In-Assign-Action))                                                 
(define-struct In-Assign-Action
  (lhs ; Variable)
   rhs ; Expression)))
   )
  #:guard
  (lambda (lhs rhs type-name)
    (unless (and (Identifier? lhs)
                 (expression? rhs))
      (error 'In-Assign-Action "Failed guard."))
    (values lhs rhs))
  #:property prop:custom-write
  (lambda (struct port write?)
    (pp-In-Assign-Action struct port)))

(define (pp-In-Assign-Action struct (port (current-output-port)))
  (fprintf port  "~a :: ~a" (In-Assign-Action-lhs struct) (In-Assign-Action-rhs struct)))

(provide (struct-out Suchthat-Assign-Action))
(define-struct Suchthat-Assign-Action
  (lhs rhs)
  #:guard
  (lambda (lhs rhs type-name)
    (unless (and (list? lhs)
                 (not (null? lhs))
                 (andmap (lambda (u) (or (Identifier? u) (typed-variable? u))) lhs))
      (error 'Suchthat-Assign-Action:guard:lhs "Guard failed, given: ~a" lhs))
    (unless (predicate? rhs)
      (error 'Suchthat-Assign-Action:guard:rhs "Guard failed, given: ~a" rhs))
    (values lhs rhs))
  #:property prop:custom-write
  (lambda (struct port write?)
    (pp-Suchthat-Assign-Action struct port)))

(define (pp-Suchthat-Assign-Action struct (port (current-output-port)))
  ;; Print lhs
  (let loop ([rest-lhs (Suchthat-Assign-Action-lhs struct)])
    (if (null? (rest rest-lhs))
        (fprintf port "~a" (first rest-lhs))
        (begin 
          (fprintf port "~a, " (first rest-lhs))
          (loop (rest rest-lhs)))))
  
  (fprintf port " :| ~a" (Suchthat-Assign-Action-rhs struct)))

(define typed-action? 
  (match-lambda
    ((struct Assign-Action (vars exprs))
     (and (andmap typed-variable? vars)
          (andmap typed-expression? exprs)))
    ((struct In-Assign-Action (var setexpr))
     (and (typed-variable? var)
          (typed-expression? setexpr)))
    ((struct Suchthat-Assign-Action (vars pred))
     (and (andmap typed-variable? vars)
          (typed-predicate? pred)))))

;                                                                 
;                                                                 
;                                                                 
;     ;;;                 ;;                          ;;          
;    ;                    ;;                          ;;          
;   ;       ;;;   ;;;;   ;;;;;   ; ;;;  ;;;     ;;;  ;;;;;   ;;;  
;   ;      ;   ;  ;;  ;   ;;     ;;        ;   ;      ;;    ;;    
;   ;      ;   ;  ;   ;   ;;     ;         ;  ;       ;;    ;;    
;   ;      ;   ;  ;   ;   ;;     ;      ;;;;  ;       ;;     ;;;  
;   ;      ;   ;  ;   ;    ;     ;     ;   ;  ;        ;        ; 
;    ;     ;   ;  ;   ;    ;     ;     ;   ;   ;       ;        ; 
;     ;;;   ;;;   ;   ;    ;;;   ;     ;;;;;    ;;;    ;;;  ;;;;  
;                                                                 
;                                                                 
;                                                                 

(provide/contract
 [struct Assign-Action ((lhs (or/c (listof Identifier?) (listof typed-variable?)))
                        (rhs (listof expression?)))]
 [typed-action? (action? . -> . boolean?)])
