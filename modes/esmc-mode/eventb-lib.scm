#lang scheme

(require scheme/match
         "../../ast/predexpr.scm"
         "../../untyped-utils.scm")

;                                                                                             
;                                                                                             
;                                                                                             
;                        ;             ;;;      ;                                       ;;;   
;    ;;;                 ;               ;                         ;;;;;                  ;   
;   ;                    ;               ;                         ;                      ;   
;   ;      ;   ; ;;;;;;  ;;;;    ;;;     ;     ;;     ;;;          ;      ;   ;   ;;;     ;   
;    ;;    ;   ; ;; ; ;  ;   ;  ;   ;    ;      ;    ;   ;         ;;;;   ;   ;      ;    ;   
;      ;;   ; ;  ;  ; ;  ;   ;  ;   ;    ;      ;    ;             ;      ;; ;    ;;;;    ;   
;       ;   ; ;  ;  ; ;  ;   ;  ;   ;    ;      ;    ;             ;       ; ;   ;   ;    ;   
;   ;   ;    ;   ;  ; ;  ;   ;  ;   ;    ;      ;    ;             ;       ;;    ;  ;;    ;   
;   ;;;;     ;   ;  ; ;  ;;;;    ;;;   ;;;;   ;;;;    ;;;          ;;;;;    ;    ;;;;;  ;;;;  
;            ;                                                                                
;          ;;                                                                                 
;                                                                                             
;; Evaluation of predicates and expressions is done case by case
;; by matching against the structures that define the AST.

(define (eval-ast ast state)
  
  (printf "eval-ast: ~a on ~a~n" ast state)
  
  (match ast
    
    ;; Evaluation of Literal Expressions
    [(struct Integer-Literal (val)) ast]
    
    [(struct Variable _)
     (let ([binding (assf (lambda (arg) (variable=? ast arg)) state)])
       (if binding
           (cdr binding)
           (error 'eval-ast/Variable
                  "No binding for variable ~a in state ~a."
                  ast 
                  state)))]
    
    ;; Evaluation of Unary Expressions
   
    ; converse: POW(a x b) -> POW(b x a)
    [(struct Expression-UnOp ('converse arg))
     (error "Unimplemented.")]
    
    ; uminus: INT -> INT
    [(struct Expression-UnOp ('uminus arg))
     (make-Integer-Literal (- (Integer-Literal-val arg)))]
    
    ; card: POW(a) -> INT
    [(struct Expression-UnOp ('card arg))
     
     (let ([argv (eval-ast arg state)])
       
       (match (eval-ast arg state)
         
         [(struct Set-Enumeration (exprs))
          (make-Integer-Literal (length exprs))]
         
         [_
          (error "No rules match to evaluate card of " argv)]))]
    
    [(struct Expression-UnOp ('pow arg))
     (error "Unimplemented.")]
    
    [(struct Expression-UnOp ('pow1 arg))
     (error "Unimplemented.")]
    
    [(struct Expression-UnOp ('union arg))
     (error "Unimplemented.")]
    
    [(struct Expression-UnOp ('inter arg))
     (error "Unimplemented.")]
    
    [(struct Expression-UnOp ('dom arg))
     (error "Unimplemented.")]
    
    [(struct Expression-UnOp ('ran arg))
     (error "Unimplemented.")]
    
    ; min: POW(INT) -> INT
    [(struct Expression-UnOp ('min arg))

     (let ([argv (eval-ast arg state)])
     
       (match (eval-ast ast state)
         
         [(struct Set-Enumeration (exprs))
          (first exprs)]
       
         [_
          (error "No rules match to evaluate min of " argv)]))]
    
    ; max: POW(INT) -> INT
    [(struct Expression-UnOp ('max arg))
          (let ([argv (eval-ast arg state)])
     
       (match (eval-ast ast state)
         
         [(struct Set-Enumeration (exprs))
          (last exprs)]
       
         [_
          (error "No rules match to evaluate min of " argv)]))]

    
    ;; Evaluation of Binary Expressions
    
    [(struct Expression-BinOp ('funimage arg1 arg2))
     (error "Unimplemented.")]
    
    [(struct Expression-BinOp ('relimage arg1 arg2))
     (error "Unimplemented.")]
    
    [(struct Expression-BinOp ('mapsto arg1 arg2))
     (error "Unimplemented.")]
    
    [(struct Expression-BinOp ('rel arg1 arg2))
     (error "Unimplemented.")]
    
    [(struct Expression-BinOp ('trel arg1 arg2))
     (error "Unimplemented.")]
    
    [(struct Expression-BinOp ('srel arg1 arg2))
     (error "Unimplemented.")]
    
    [(struct Expression-BinOp ('pfun arg1 arg2))
     (error "Unimplemented.")]
    
    [(struct Expression-BinOp ('tfun arg1 arg2))
     (error "Unimplemented.")]
    
    [(struct Expression-BinOp ('pinj arg1 arg2))
     (error "Unimplemented.")]
    
    [(struct Expression-BinOp ('tinj arg1 arg2))
     (error "Unimplemented.")]
    
    [(struct Expression-BinOp ('psur arg1 arg2))
     (error "Unimplemented.")]
    
    [(struct Expression-BinOp ('tsur arg1 arg2))
     (error "Unimplemented.")]
    
    [(struct Expression-BinOp ('tbij arg1 arg2))
     (error "Unimplemented.")]
    
    [(struct Expression-BinOp ('bunion arg1 arg2))
     (error "Unimplemented.")]
    
    [(struct Expression-BinOp ('binter arg1 arg2))
     (error "Unimplemented.")]
    
    [(struct Expression-BinOp ('setminus arg1 arg2))
     (error "Unimplemented.")]
    
    [(struct Expression-BinOp ('cprod arg1 arg2))
     (error "Unimplemented.")]
    
    [(struct Expression-BinOp ('dprod arg1 arg2))
     (error "Unimplemented.")]
    
    [(struct Expression-BinOp ('pprod arg1 arg2))
     (error "Unimplemented.")]
    
    [(struct Expression-BinOp ('bcomp arg1 arg2))
     (error "Unimplemented.")]
    
    [(struct Expression-BinOp ('fcomp arg1 arg2))
     (error "Unimplemented.")]
    
    [(struct Expression-BinOp ('ovl arg1 arg2))
     (error "Unimplemented.")]
    
    [(struct Expression-BinOp ('domres arg1 arg2))
     (error "Unimplemented.")]
    
    [(struct Expression-BinOp ('domsub arg1 arg2))
     (error "Unimplemented.")]
    
    [(struct Expression-BinOp ('ranres arg1 arg2))
     (error "Unimplemented.")]
    
    [(struct Expression-BinOp ('ransub arg1 arg2))
     (error "Unimplemented.")]
    
    [(struct Expression-BinOp ('upto arg1 arg2))
     (make-Set-Enumeration 
      (map make-Integer-Literal
           (build-list (+ 1 (- (Integer-Literal-val arg2)
                               (Integer-Literal-val arg1)))
                       (lambda (x) (+ x (Integer-Literal-val arg1))))))]
    
    ; plus: INT x INT -> INT
    [(struct Expression-BinOp ('plus arg1 arg2))
     (make-Integer-Literal (+ (Integer-Literal-val arg1)
                              (Integer-Literal-val arg2)))]
    
    ; minus: INT x INT -> INT
    [(struct Expression-BinOp ('minus arg1 arg2))
     (make-Integer-Literal (- (Integer-Literal-val arg1)
                              (Integer-Literal-val arg2)))]
    
    ; mul: INT x INT -> INT
    [(struct Expression-BinOp ('mul arg1 arg2))
     (make-Integer-Literal (* (Integer-Literal-val arg1)
                              (Integer-Literal-val arg2)))]
    
    ; div: INT x INT -> INT
    [(struct Expression-BinOp ('div arg1 arg2))
     (make-Integer-Literal (quotient (Integer-Literal-val arg1)
                                     (Integer-Literal-val arg2)))]
    
    ; mod: INT x INT -> INT
    [(struct Expression-BinOp ('mod arg1 arg2))
     (make-Integer-Literal (modulo (Integer-Literal-val arg1)
                                   (Integer-Literal-val arg2)))]
    
    ; expn: INT x INT -> INT
    [(struct Expression-BinOp ('expn arg1 arg2))
     (make-Integer-Literal (expt (Integer-Literal-val arg1)
                                 (Integer-Literal-val arg2)))]
    
    
;                                     
;                                     
;                                     
;                            ;        
;                            ;        
;                            ;        
;   ;;;;   ; ;;;   ;;;    ;;;;   ;;;  
;   ;   ;  ;;     ;   ;  ;   ;  ;     
;   ;   ;  ;      ;;;;;  ;   ;   ;;   
;   ;   ;  ;      ;      ;   ;     ;; 
;   ;   ;  ;      ;      ;  ;;      ; 
;   ;;;;   ;       ;;;    ;;;;  ;;;;  
;   ;                                 
;   ;                                 
;                                     

    [(struct Predicate-Literal (lit)) ast]
    
    [(struct Predicate-UnOp ('not arg))
     
     (match (eval-ast arg state)
       
       [(struct Predicate-Literal (lit))
        (if (eqv? lit 'bfalse)
            (make-Predicate-Literal 'btrue)
            (make-Predicate-Literal 'bfalse))]
       
       [_
        (error 'eval-ast/Predicate-UnOp/not 
               "Unexpected arg")])]
    
    [(struct Predicate-BinOp (op arg1 arg2))
     
     (let ([earg1 (eval-ast arg1 state)]
           [earg2 (eval-ast arg2 state)])
       
       (case op
         [(land) 
          (match (cons earg1 earg2)
            [(cons (struct Predicate-Literal ('btrue))
                   (struct Predicate-Literal ('btrue)))
             earg1]
            [_ (make-Predicate-Literal ('bfalse))])]

         [(lor) 
          (match (cons earg1 earg2)
            [(cons (struct Predicate-Literal ('bfalse))
                   (struct Predicate-Literal ('bfalse)))
             earg1]
            [_ (make-Predicate-Literal ('btrue))])]
         
         [(limp) 
          (match (cons earg1 earg2)
            [(cons (struct Predicate-Literal ('btrue))
                   (struct Predicate-Literal ('bfalse)))
             earg2]
            [_ (make-Predicate-Literal ('btrue))])]
         
         [(leqv) 
          (match (list earg1 earg2)
            [(list-no-order (struct Predicate-Literal ('bfalse))
                            (struct Predicate-Literal ('btrue)))
             (make-Predicate-Literal ('bfalse))]
            [_ (make-Predicate-Literal ('btrue))])]))]
             
    
    [(struct Predicate-RelOp (op arg1 arg2))
     
     (let* ([earg1 (eval-ast arg1 state)]
            [earg2 (eval-ast arg2 state)])
       
       (if (Integer-Literal? earg2)
           (let ([int1 (Integer-Literal-val earg1)]
                 [int2 (Integer-Literal-val earg2)])
             (if (or (and (eqv? op 'equal) (= int1 int2))
                     (and (eqv? op 'notequal) (not (= int1 int2)))
                     (and (eqv? op 'lt) (< int1 int2))
                     (and (eqv? op 'le) (<= int1 int2))
                     (and (eqv? op 'gt) (> int1 int2))
                     (and (eqv? op 'ge) (>= int1 int2)))
                 (make-Predicate-Literal 'btrue)
                 (make-Predicate-Literal 'bfalse)))
           
           (error 'eval-ast/Predicate-Rel
                  "Unimplemented arguments to Predicate RelOp (~a, ~a)"
                  earg1 earg2)))]
                
    [_
     (error 'eval-ast 
            "No rules match to evaluate: ~a on state ~a" ast state)]))


;                              
;                              
;                        ;;;   
;   ;;;;;;                 ;   
;   ;                      ;   
;   ;      ;   ;   ;;;     ;   
;   ;      ;   ;  ;   ;    ;   
;   ;;;;;;  ; ;       ;    ;   
;   ;       ; ;    ;;;;    ;   
;   ;       ; ;   ;   ;    ;   
;   ;        ;    ;   ;    ;   
;   ;;;;;;   ;     ;; ;     ;; 
;                              
;                              
;                          ;   
;
(provide eval-expression
         eval-predicate)

(define-namespace-anchor a)
(define this-namespace (namespace-anchor->namespace a))

(define eval-expression
  (case-lambda 
    [(expr state1 state2 . states)
     (eval-expression expr (apply dict-merge state1 state2 states))]
    [(expr state)
     (eval-ast expr state)]))

(define eval-predicate 
  (case-lambda
    [(expr state1 state2 . states)
     (eval-predicate expr (apply dict-merge state1 state2 states))]
    [(expr state)
     (let ([val (eval-ast expr state)])
       (match val
         [(struct Predicate-Literal ('btrue)) #t]
         [_ #f]))]))
;
;
;                                                                                
;                                                     ;                          
;    ;;;;  ;           ;                ;;;;;                    ;               
;   ;;   ; ;           ;                ;   ;;                   ;               
;   ;    ;;;;;  ;;;; ;;;;;   ;;;        ;    ; ;;;; ;;;   ;;;; ;;;;;   ;;;   ;;;;
;    ;;;   ;        ;  ;    ;   ;       ;   ;; ;; ;   ;   ;   ;  ;    ;   ;  ;; ;
;      ;;; ;     ;;;;  ;    ;;;;;       ;;;;;  ;      ;   ;   ;  ;    ;;;;;  ;   
;        ; ;    ;   ;  ;    ;           ;      ;      ;   ;   ;  ;    ;      ;   
;   ;    ; ;    ;  ;;  ;    ;;          ;      ;      ;   ;   ;  ;    ;;     ;   
;    ;;;;  ;;;   ;;;;  ;;;   ;;;;       ;      ;    ;;;;; ;   ;  ;;;   ;;;;  ;   
;                                                                                
;                                                                                
;                                                                                


(define (print-state state (out (current-output-port)))
    (parameterize ([current-output-port out])
      (for-each (match-lambda ((cons var val) (printf "~a : ~a" var val)))
                state)))