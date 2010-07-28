#lang racket/base

(require racket/match
         racket/contract
         racket/list
         (only-in srfi/1 
                  find 
                  lset-union
                  lset-difference)
         "../../ast/predexpr.rkt"
         "state.rkt"
         "value-generator.rkt"
         "../../untyped-utils.rkt")

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

(define/contract (eval-ast ast state)
  ((or/c expr/wot? predicate?) state? . -> . (or/c expr/wot? predicate?))
  
  (parameterize 
      ([match-equality-test expression/wot=])
    (match ast
      
      ;; Evaluation of Literal Expressions
      [(struct Integer-Literal (val)) ast]
      
      [(or (struct Variable _)
           (struct Constant _)
           (struct Set _))
       (let ([binding (state-ref state ast)])
         (if binding
             binding
             (error 'eval-ast/Literal
                    "No binding for literal ~a in state ~a."
                    ast 
                    state)))]
      
      [(struct Set-Literal _) ast]
      
      [(struct Expression-Literal _) ast]
      
      ;; Set Enumeration
      [(struct Set-Enumeration (exprs))
       (make-Set-Enumeration (map (lambda (expr) 
                                    (eval-ast expr state))
                                  exprs))]
      
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
       (let ([earg (eval-ast arg state)])
         (make-Expression-UnOp 'pow earg))]
      
      [(struct Expression-UnOp ('pow1 arg))
       (error "Unimplemented pow1.")]
      
      [(struct Expression-UnOp ('union arg))
       (error "Unimplemented union.")]
      
      [(struct Expression-UnOp ('inter arg))
       (error "Unimplemented inter.")]
      
      
      ;                       
      ;                       
      ;                       
      ;       ;               
      ;       ;               
      ;       ;               
      ;    ;;;;   ;;;  ;;;;;; 
      ;   ;   ;  ;   ; ;; ; ; 
      ;   ;   ;  ;   ; ;  ; ; 
      ;   ;   ;  ;   ; ;  ; ; 
      ;   ;  ;;  ;   ; ;  ; ; 
      ;    ;;;;   ;;;  ;  ; ; 
      ;                       
      ;                       
      ;               ;       
      
      [(struct Expression-UnOp ('dom arg))
       
       (let ([argv (eval-ast arg state)])
         
         (match argv
           [(struct Expression-Literal ('emptyset)) argv]
           [(struct Set-Enumeration ((list (struct Expression-BinOp ('mapsto cars _)) ...)))
            (make-Set-Enumeration cars)]
           [_
            (error "Unimplemented dom.")]))]
      
      [(struct Expression-UnOp ('ran arg))
       
       (let ([argv (eval-ast arg state)])
         
         (match argv
           [(struct Expression-Literal ('emptyset)) argv]
           [(struct Set-Enumeration ((list (struct Expression-BinOp ('mapsto _ cdrs)) ...)))
            (make-Set-Enumeration cdrs)]
           [_
            (error "Unimplemented ran.")]))]
      
      ; min: POW(INT) -> INT
      [(struct Expression-UnOp ('min arg))
       
       (let ([argv (eval-ast arg state)])
         
         (match argv
           
           [(struct Set-Enumeration (exprs))
            (make-Integer-Literal (apply min (map Integer-Literal-val exprs)))]
           
           [_
            (error "No rules match to evaluate min of " argv)]))]
      
      ; max: POW(INT) -> INT
      [(struct Expression-UnOp ('max arg))
       (let ([argv (eval-ast arg state)])
         
         (match argv
           
           [(struct Set-Enumeration (exprs))
            (make-Integer-Literal (apply max (map Integer-Literal-val exprs)))]
           
           [_
            (error "No rules match to evaluate min of " argv)]))]
      
      
      ;; Evaluation of Binary Expressions
      
      
      ;                                                          
      ;                                                          
      ;                                                          
      ;     ;;;;                 ;                               
      ;    ;;                                                    
      ;    ;                                                     
      ;   ;;;;;  ;   ;  ; ;;;   ;;    ;;;;;;  ;;;    ;;;;;  ;;;  
      ;    ;     ;   ;  ;;  ;    ;    ; ;; ;     ;  ;   ;  ;   ; 
      ;    ;     ;   ;  ;   ;    ;    ; ;  ;     ;  ;   ;  ;   ; 
      ;    ;     ;   ;  ;   ;    ;    ; ;  ;  ;;;;   ;;;   ;;;;; 
      ;    ;     ;   ;  ;   ;    ;    ; ;  ; ;   ;  ;      ;     
      ;    ;     ;   ;  ;   ;    ;    ; ;  ; ;   ;  ;      ;     
      ;    ;      ;;;;  ;   ;   ;;;;  ; ;  ; ;;;;;  ;;;;;   ;;;; 
      ;                                             ;   ;;       
      ;                                             ;;;;;        
      ;                                                          
      ;; In ('funimage arg1 arg2), there is a constraint
      ;; arg2 : dom(arg1)
      [(struct Expression-BinOp ('funimage arg1 arg2))
       
       (let ([earg1 (eval-ast arg1 state)]
             [earg2 (eval-ast arg2 state)])
         
         (match (cons earg1 earg2)
           [(cons (struct Expression-Literal ('emptyset)) _)
            (raise 'fail-funimage)]
           
           [(cons (struct Set-Enumeration (exprs)) val)
            (let ([pair (find (match-lambda 
                                [(struct Expression-BinOp ('mapsto domval _))
                                 (expression/wot= domval val)])
                              exprs)])
              (if pair
                  (Expression-BinOp-arg2 pair)
                  (raise 'fail-funimage)))]
           
           [_
            (error 'Expression-BinOp/funimage "Unimplemented funimage for args ~a, ~a." earg1 earg2)]))]
      
      
      ;                                                                        
      ;                                                                        
      ;                                                                        
      ;                               ;;;      ;                               
      ;                                 ;                                      
      ;                                 ;                                      
      ;                 ; ;;;   ;;;     ;     ;;   ;;;;;;   ;;;    ;;;;   ;;;  
      ;   ;;;;;         ;;     ;   ;    ;      ;   ;; ; ;      ;  ;  ;   ;   ; 
      ;                 ;      ;;;;;    ;      ;   ;  ; ;   ;;;;  ;  ;   ;;;;; 
      ;                 ;      ;        ;      ;   ;  ; ;  ;   ;  ;;;    ;     
      ;                 ;      ;        ;      ;   ;  ; ;  ;  ;;  ;      ;     
      ;                 ;       ;;;   ;;;;   ;;;;  ;  ; ;  ;;;;;  ;;;;    ;;;  
      ;                                                           ;   ;        
      ;                                                           ;;;;         
      ;                                                                        
      
      [(struct Expression-BinOp ('relimage arg1 arg2))
       (error "Unimplemented relimage.")]
      
      [(struct Expression-BinOp ('mapsto arg1 arg2))
       (let ([earg1 (eval-ast arg1 state)]
             [earg2 (eval-ast arg2 state)])
         (make-Expression-BinOp 'mapsto earg1 earg2))]
      
      [(struct Expression-BinOp ('rel arg1 arg2))
       (error "Unimplemented rel.")]
      
      [(struct Expression-BinOp ('trel arg1 arg2))
       (error "Unimplemented trel.")]
      
      [(struct Expression-BinOp ('srel arg1 arg2))
       (error "Unimplemented srel.")]
      
      [(struct Expression-BinOp ('pfun arg1 arg2))
       (make-Expression-BinOp 'pfun 
                              (eval-ast arg1 state)
                              (eval-ast arg2 state))]
      
      [(struct Expression-BinOp ('tfun arg1 arg2))
       (make-Expression-BinOp 'tfun
                              (eval-ast arg1 state)
                              (eval-ast arg2 state))]
      
      [(struct Expression-BinOp ('pinj arg1 arg2))
       (make-Expression-BinOp 'pinj
                              (eval-ast arg1 state)
                              (eval-ast arg2 state))]
      
      [(struct Expression-BinOp ('tinj arg1 arg2))
       (make-Expression-BinOp 'tinj
                              (eval-ast arg1 state)
                              (eval-ast arg2 state))]
      
      [(struct Expression-BinOp ('psur arg1 arg2))
       (error "Unimplemented psur.")]
      
      [(struct Expression-BinOp ('tsur arg1 arg2))
       (error "Unimplemented tsur.")]
      
      [(struct Expression-BinOp ('tbij arg1 arg2))
       (error "Unimplemented tbij.")]
      
      [(struct Expression-BinOp ('bunion arg1 arg2))
       
       (let ([earg1 (eval-ast arg1 state)]
             [earg2 (eval-ast arg2 state)])
         
         (match (cons earg1 earg2)
           [(cons _ (struct Expression-Literal ('emptyset))) earg1]
           [(cons (struct Expression-Literal ('emptyset)) _) earg2]
           
           [(cons (struct Set-Enumeration (exprs1))
                  (struct Set-Enumeration (exprs2)))
            (make-Set-Enumeration (lset-union expression/wot= exprs1 exprs2))]
           [_
            (error 'eval-ast/bunion
                   "Cannot compute: ~a \\/ ~a" earg1 earg2)]))]
      
      [(struct Expression-BinOp ('binter arg1 arg2))
       
       (let ([earg1 (eval-ast arg1 state)]
             [earg2 (eval-ast arg2 state)])
         
         (match (cons earg1 earg2)
           
           [_
            (error 'eval-ast/binter 
                   "Cannot compute: ~a /\\ ~a" earg1 earg2)]))]
      
      ;                                                                        
      ;                                                                        
      ;                                               ;                        
      ;                                 ;                                      
      ;                                 ;                                      
      ;                  ;;;    ;;;   ;;;;;  ;;;;;  ;;;    ;;;;   ;   ;   ;;;  
      ;                 ;   ;  ;;  ;    ;    ; ; ;    ;    ;;  ;  ;   ;  ;   ; 
      ;                 ;      ;   ;    ;    ; ; ;    ;    ;   ;  ;   ;  ;     
      ;    ;;;           ;;;   ;;;;;    ;    ; ; ;    ;    ;   ;  ;   ;   ;;;  
      ;                     ;  ;        ;    ; ; ;    ;    ;   ;  ;   ;      ; 
      ;                 ;   ;  ;;  ;    ;    ; ; ;    ;    ;   ;  ;  ;;  ;   ; 
      ;                  ;;;    ;;;     ;;;  ; ; ;  ;;;;;  ;   ;   ;;;;   ;;;  
      ;                                                                        
      ;                                                                        
      ;                                                                        
      
      [(struct Expression-BinOp ('setminus arg1 arg2))
       
       (let ([earg1 (eval-ast arg1 state)]
             [earg2 (eval-ast arg2 state)])
         
         (match (cons earg1 earg2)
           [(cons _ (struct Expression-Literal ('emptyset))) earg1]
           
           [(cons (struct Expression-Literal ('emptyset)) _) earg1]
           
           [(cons (struct Set-Enumeration (exprs1))
                  (struct Set-Enumeration (exprs2)))
            (make-Set-Enumeration (lset-difference expression/wot= exprs1 exprs2))]
           
           [(cons (struct Expression-Literal ('integer)) _)
            (make-Expression-BinOp 'setminus earg1 earg2)]
           
           ;; (S \ {k1, k2 ...}) \ {p1, p2, ...} = S1 \ (K \/ P)
           [(cons (struct Expression-BinOp ('setminus S (and K (struct Set-Enumeration (_)))))
                  (and P (struct Set-Enumeration (_))))
            (make-Expression-BinOp 'setminus
                                   S
                                   (eval-ast (make-Expression-BinOp 'bunion K P)
                                             state))]
           [_
            (error 'eval-ast/Expression-BinOp/setminus
                   "Cannot evaluate expression ~a \\ ~a" earg1 earg2)]))]
      
      [(struct Expression-BinOp ('cprod arg1 arg2))
       
       (let ([earg1 (eval-ast arg1 state)]
             [earg2 (eval-ast arg2 state)])
         
         (match (cons earg1 earg2)
           
           [(or (cons (struct Expression-Literal ('emptyset)) _)
                (cons _ (struct Expression-Literal ('emptyset))))
            (make-Expression-Literal 'emptyset)]
           
           [(cons (struct Set-Enumeration (exprs1))
                  (struct Set-Enumeration (exprs2)))
            
            (let loop1 ([e1 exprs1] [total '()])
              
              (if (null? e1)
                  (if (null? total)
                      (make-Expression-Literal 'emptyset)
                      (make-Set-Enumeration total))
                  (loop1 (rest exprs1)
                         (append total 
                                 (let ([e1val (first e1)])
                                   (let loop2 ([e2 exprs2] [acum '()])
                                     (if (null? e2) 
                                         acum
                                         (loop2 (rest e2)
                                                (cons (make-Expression-BinOp 'mapsto
                                                                             e1val
                                                                             (first e2))
                                                      acum)))))))))]
           
           [_
            (error 'eval-ast/Expression-BinOp/cprod
                   "Cannot compute: ~a ** ~a" earg1 earg2)]))]
      
      
      [(struct Expression-BinOp ('dprod arg1 arg2))
       (error "Unimplemented dprod.")]
      
      [(struct Expression-BinOp ('pprod arg1 arg2))
       (error "Unimplemented pprod.")]
      
      [(struct Expression-BinOp ('bcomp arg1 arg2))
       (error "Unimplemented bcomp.")]
      
      [(struct Expression-BinOp ('fcomp arg1 arg2))
       
       (let ([earg1 (eval-ast arg1 state)]
             [earg2 (eval-ast arg2 state)])
         
         (match (cons earg1 earg2)
           
           [(or (cons (struct Expression-Literal ('emptyset)) _)
                (cons _ (struct Expression-Literal ('emptyset))))
            (make-Expression-Literal 'emptyset)]
           
           [(cons (struct Set-Enumeration (exprs1))
                  (struct Set-Enumeration (exprs2)))
            (make-Set-Enumeration
             (let loop1 ([e1 exprs1] [final '()])
               (if (null? e1)
                   final
                   (loop1 (rest e1) 
                          (append final 
                                  (let loop2 ([e2 exprs2] [acum '()])
                                    (cond [(null? e2) acum]
                                          [(expression/wot= (Expression-BinOp-arg2 (first e1))
                                                            (Expression-BinOp-arg1 (first e2)))
                                           (loop2 (rest e2) (cons (make-Expression-BinOp 'mapsto 
                                                                                         (Expression-BinOp-arg1 (first e1))
                                                                                         (Expression-BinOp-arg2 (first e2)))
                                                                  acum))]
                                          [else (loop2 (rest e2) acum)])))))))]
           [_         
            (error "Unimplemented fcomp.")]))]
      
      
      ;                       
      ;                       
      ;                 ;;;   
      ;                   ;   
      ;                   ;   
      ;    ;;;   ;   ;    ;   
      ;   ;; ;;  ;   ;    ;   
      ;   ;   ;   ; ;     ;   
      ;   ;   ;   ; ;     ;   
      ;   ;   ;   ;;;     ;   
      ;   ;; ;;   ;;;     ;   
      ;    ;;;     ;       ;; 
      ;                       
      ;                       
      ;               ;       
      
      
      [(struct Expression-BinOp ('ovl arg1 arg2))
       
       (let ([earg1 (eval-ast arg1 state)]
             [earg2 (eval-ast arg2 state)])
         
         (eval-ast 
          (make-Expression-BinOp 'bunion
                                 (make-Expression-BinOp 'domsub
                                                        (make-Expression-UnOp 'dom earg2)
                                                        earg1)
                                 earg2)
          state))]
      
      
      ;                                            
      ;                                            
      ;       ;                                    
      ;       ;                                    
      ;       ;                                    
      ;    ;;;;   ;;;   ;;;;;   ;;;;   ;;;    ;;;  
      ;   ;; ;;  ;; ;;  ; ; ;   ;;  ; ;;  ;  ;   ; 
      ;   ;   ;  ;   ;  ; ; ;   ;     ;   ;  ;     
      ;   ;   ;  ;   ;  ; ; ;   ;     ;;;;;   ;;;  
      ;   ;   ;  ;   ;  ; ; ;   ;     ;          ; 
      ;   ;; ;;  ;; ;;  ; ; ;   ;     ;;  ;  ;   ; 
      ;    ;;;;   ;;;   ; ; ;   ;      ;;;    ;;;  
      ;                                            
      ;                                            
      ;                                            
      
      [(struct Expression-BinOp ('domres arg (struct Expression-Literal ('id))))
       
       (match arg
         [(struct Expression-Literal ('emptyset)) arg]
         
         [(struct Set-Enumeration (exprs))
          (make-Set-Enumeration 
           (map (lambda (e) (make-Expression-BinOp 'mapsto e e)) exprs))]
         [_
          (error 'Expression-BinOp/domres/id "Can't compute : ~a <| id" arg)])]
      
      [(struct Expression-BinOp ('domres arg1 arg2))
       (let ([earg1 (eval-ast arg1 state)]
             [earg2 (eval-ast arg2 state)])
         (eval-ast 
          (make-Expression-BinOp 'fcomp
                                 (make-Expression-BinOp 'domres 
                                                        earg1
                                                        (make-Expression-Literal 'id))
                                 earg2)
          state))]
      
      [(struct Expression-BinOp ('domsub arg1 arg2))
       (let ([earg1 (eval-ast arg1 state)]
             [earg2 (eval-ast arg2 state)])
         (eval-ast
          (make-Expression-BinOp 'domres
                                 (make-Expression-BinOp 'setminus
                                                        (make-Expression-UnOp 'dom earg2)
                                                        earg1)
                                 earg2)
          state))]
      
      [(struct Expression-BinOp ('ranres arg1 arg2))
       (let ([earg1 (eval-ast arg1 state)]
             [earg2 (eval-ast arg2 state)])
         (eval-ast
          (make-Expression-BinOp 'fcomp
                                 earg1
                                 (make-Expression-BinOp 'domres 
                                                        earg2
                                                        (make-Expression-Literal 'id)))
          state))]
      
      [(struct Expression-BinOp ('ransub arg1 arg2))
       (let ([earg1 (eval-ast arg1 state)]
             [earg2 (eval-ast arg2 state)])
         (eval-ast 
          (make-Expression-BinOp 'ranres 
                                 earg1
                                 (make-Expression-BinOp 'setminus
                                                        (make-Expression-UnOp 'ran earg1)
                                                        earg2))
          state))]
      
      
      ;                                            
      ;                                            
      ;                                            
      ;                                            
      ;                                            
      ;                                ;           
      ;                 ;   ;  ;;;;   ;;;;    ;;;  
      ;   ;;;;;         ;   ;  ;   ;   ;     ;   ; 
      ;                 ;   ;  ;   ;   ;     ;   ; 
      ;                 ;   ;  ;   ;   ;     ;   ; 
      ;                 ;  ;;  ;   ;   ;     ;   ; 
      ;                  ;;;;  ;;;;     ;;;   ;;;  
      ;                        ;                   
      ;                        ;                   
      ;                                            
      
      
      [(struct Expression-BinOp ('upto arg1 arg2))
       
       (let* ([earg1 (eval-ast arg1 state)]
              [earg2 (eval-ast arg2 state)]
              [numvals (+ 1 (- (Integer-Literal-val earg2)
                               (Integer-Literal-val earg1)))])
         
         (when (< numvals 0)
           (raise 'fail-upto))
         
         (let ([int-list (map make-Integer-Literal
                              (build-list numvals
                                          (lambda (x) (+ x (Integer-Literal-val earg1)))))])
           
           (if (null? int-list)
               (make-Expression-Literal 'emptyset)
               (make-Set-Enumeration int-list))))]
      
      [(struct Expression-BinOp ((or 'plus 'minus 'mul 'div 'mod 'expn) arg1 arg2))
       
       (let ([earg1 (eval-ast arg1 state)]
             [earg2 (eval-ast arg2 state)])
         
         (case (Expression-BinOp-op ast)
           [(plus) ; plus: INT x INT -> INT
            (make-Integer-Literal (+ (Integer-Literal-val earg1)
                                     (Integer-Literal-val earg2)))]
           
           
           [(minus) ; minus: INT x INT -> INT   
            (make-Integer-Literal (- (Integer-Literal-val earg1)
                                     (Integer-Literal-val earg2)))]
           
           [(mul); mul: INT x INT -> INT
            (make-Integer-Literal (* (Integer-Literal-val earg1)
                                     (Integer-Literal-val earg2)))]
           
           [(div) ; div: INT x INT -> INT
            (make-Integer-Literal (quotient (Integer-Literal-val earg1)
                                            (Integer-Literal-val earg2)))]
           
           [(mod) ; mod: INT x INT -> INT
            (make-Integer-Literal (modulo (Integer-Literal-val earg1)
                                          (Integer-Literal-val earg2)))]
           
           [(expn) ; expn: INT x INT -> INT
            (make-Integer-Literal (expt (Integer-Literal-val earg1)
                                        (Integer-Literal-val earg2)))]
           
           [else 
            (error "Unreachable")]))]
      
      
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
              ebtrue
              ebfalse)]
         
         [_
          (error 'eval-ast/Predicate-UnOp/not 
                 "Unexpected arg")])]
      
      [(struct Predicate-BinOp (op arg1 arg2))
       
       (let ([earg1 (eval-ast arg1 state)])
         
         (case op
           [(land) 
            (if (ebfalse? earg1)
                ebfalse
                (eval-ast arg2 state))]
           
           [(lor) 
            (if (ebfalse? earg1)
                (eval-ast arg2 state)
                ebtrue)]
           
           [(limp) 
            (if (ebfalse? earg1)
                ebtrue
                (eval-ast arg2 state))]
           
           [(leqv) 
            (let ([earg2 (eval-ast arg1 state)])
              (match (list earg1 earg2)
                [(list-no-order (struct Predicate-Literal ('bfalse))
                                (struct Predicate-Literal ('btrue)))
                 ebfalse]
                [_ ebtrue]))]))]
      
      [(struct Predicate-RelOp ('notequal arg1 arg2))
       (eval-ast (make-Predicate-UnOp 'not (make-Predicate-RelOp 'equal arg1 arg2)) 
                 state)]
      
      [(struct Predicate-RelOp ('notin arg1 arg2))
       (eval-ast (make-Predicate-UnOp 'not (make-Predicate-RelOp 'in arg1 arg2)) 
                 state)]
      
      [(struct Predicate-RelOp ((or 'lt 'le 'gt 'ge) arg1 arg2))
       
       (let* ([op (Predicate-RelOp-op ast)]
              [earg1 (eval-ast arg1 state)]
              [earg2 (eval-ast arg2 state)]
              [int1 (Integer-Literal-val earg1)]
              [int2 (Integer-Literal-val earg2)])
         (if (or (and (eqv? op 'lt) (< int1 int2))
                 (and (eqv? op 'le) (<= int1 int2))
                 (and (eqv? op 'gt) (> int1 int2))
                 (and (eqv? op 'ge) (>= int1 int2)))
             ebtrue
             ebfalse))]
      
      [(struct Predicate-RelOp ('equal arg1 arg2)) 
       (let ([earg1 (eval-ast arg1 state)]
             [earg2 (eval-ast arg2 state)])
         
         (if (expression/wot= earg1 earg2)
             ebtrue
             ebfalse))]
      
      
      ;                              
      ;                              
      ;                              
      ;                   ;          
      ;                              
      ;                              
      ;                  ;;    ;;;;  
      ;   ;;;;;           ;    ;   ; 
      ;                   ;    ;   ; 
      ;                   ;    ;   ; 
      ;                   ;    ;   ; 
      ;                 ;;;;   ;   ; 
      ;                              
      ;                              
      ;                              
      
      [(struct Predicate-RelOp ('in arg1 arg2))
       
       (let ([earg1 (eval-ast arg1 state)]
             [earg2 (eval-ast arg2 state)])
         
         (match (cons earg1 earg2)
           
           ; {} in _
           [(cons (struct Expression-Literal ('emptyset)) _)
            ebtrue]
           
           ; _ in {}
           [(cons _ (struct Expression-Literal ('emptyset)))
            ebfalse]
           
           ; _ in INT
           [(cons _ (struct Expression-Literal ('integer)))
            ebtrue]
           
           ; _ in POW(INT)
           [(cons _ (struct Expression-UnOp ('pow (struct Expression-Literal ('integer)))))
            ebtrue]
           
           ; _ in NAT
           [(cons _ (struct Expression-Literal ('natural)))
            (if (>= (Integer-Literal-val earg1) 0)
                ebtrue
                ebfalse)]
           
           ; _ in {e1, e2, ...}
           [(cons _ (struct Set-Enumeration (exprs)))
            (if (ormap (lambda (x) (expression/wot= earg1 x)) exprs)
                ebtrue
                ebfalse)]
           
           ; {} in POW(_)
           [(cons (struct Expression-Literal ('emptyset))
                  (struct Expression-UnOp ('pow _)))
            ebtrue]
           
           [(cons (struct Expression-Literal ('integer))
                  (struct Expression-UnOp ('pow (struct Expression-Literal ('integer)))))
            ebtrue]
           
           ; {} in BOOL
           [(cons _ (struct Expression-Literal ('bool)))
            ebtrue]
           
           ; {e1, e2, ...} in POW({k1, k2, ...})
           [(cons (struct Set-Enumeration (exprs1))
                  (struct Expression-UnOp ('pow (struct Set-Enumeration (exprs2)))))
            (if (andmap (lambda (expr1)
                          (find (lambda (expr2) (expression/wot= expr1 expr2))
                                exprs2))
                        exprs1)
                ebtrue
                ebfalse)]
           
           ; {e1, e2, ...} in fundom (or +> --> >->) funran
           ; We need to check that the set {e1, e2, ...} is a partial function from fundom to funran
           ; this means that:
           ; - first(ek) in fundom
           ; - second(ek) in funran
           ; - there are no duplicates in first(ek). [function constraint]
           [(cons (struct Set-Enumeration (exprs))
                  (struct Expression-BinOp ((or 'pfun 'tfun 'tinj) fundom funran)))
            (if (and (andmap (lambda (ek)
                               (match ek
                                 [(struct Expression-BinOp ('mapsto arg1 arg2))
                                  (eband (eval-ast (make-Predicate-RelOp 'in arg1 fundom)
                                                   state)
                                         (eval-ast (make-Predicate-RelOp 'in arg2 funran)
                                                   state))]))
                             exprs)
                     (= (length exprs)
                        (length (remove-duplicates exprs 
                                                   expression/wot= 
                                                   #:key Expression-BinOp-arg1))))
                ebtrue
                ebfalse)]
           
           [(cons (struct Expression-BinOp ('setminus set _))
                  (struct Expression-UnOp ('pow set)))
            ebtrue]
           
           [(cons el (struct Expression-BinOp ('setminus set1 set2)))
            (eval-ast (make-Predicate-BinOp 'land
                                            (make-Predicate-RelOp 'in el set1)
                                            (make-Predicate-RelOp 'notin el set2))
                      state)]
           
           [else
            (error 'eval-ast/Rel-Op/in
                   "Can't evaluate ~a in ~a" earg1 earg2)]))]
      
      [_
       (error 'eval-ast 
              "No rules match to evaluate: ~a on state ~a" ast state)])))

;(require "../../types.rkt")
;  (define t 
;    (eval-predicate/quantified
;     (make-Quantifier 
;      'forall
;      (make-Expr/wt 
;       (make-Type-Integer)
;       (make-Variable 'x))
;     (make-Predicate-UnOp 'not
;      (make-Quantifier
;       'forall
;       (make-Expr/wt
;        (make-Type-Integer)
;        (make-Variable 'y))
;       (make-Predicate-BinOp
;        'land
;        (make-Predicate-RelOp
;         'equal 
;         (make-Variable 'x)
;         (make-Variable 'x))
;        (make-Predicate-RelOp
;         'equal
;         (make-Variable 'y)
;         (make-Integer-Literal 100))))))
;     (make-state)))
(define (eval-predicate/quantified ast state)
  (if (not (quantified-predicate? ast))
      (eval-ast ast state)
      (letrec ([rip-quantvars
                ;; Rips a list of variables that are quantified
                ;; and the resulting predicate
                (match-lambda
                  [(struct Quantifier ('forall var body))
                   (let-values ([(vars pred) (rip-quantvars body)])
                     (values (cons var vars)
                             pred))]
                  [pred (values '() pred)])])
        (let ([neg? (match ast [(struct Predicate-UnOp ('not (struct Quantifier ('forall _ _)))) #t] [_ #f])])
          (let-values ([(vars bare-pred) (rip-quantvars 
                                          (if neg?
                                              (Predicate-UnOp-arg ast)
                                              ast))])
            (thread (lambda ()
                      (let ([thread-id (gensym 'thread:)])
                        (letrec ([handle-msgs
                                  (lambda (subthread)
                                    ;; Check mailbox
                                    (let ([msg (thread-try-receive)])
                                      (when msg
                                        (match msg
                                          [(list t 'cover)
                                           (let ([total-count (if subthread
                                                                  (begin (thread-send subthread (list (current-thread) 'cover))
                                                      ;;; XXX... Continue... MESSAGE (PARSING AND CONSTRUCTION) HANDLING IS PRETTY MESSY!!!
                                           (thread-send t (cons msg count))]
                                          [(list t 'name) (thread-send t (cons msg thread-id))]))))])
                        (let ([enum (type-list-enumerator (map Expr/wt-type vars))]
                              [untyped-vars (map Expr/wt-expr vars)])
                          (let loop ([next-enum (enum)] [next-prt (enum 'prt)] [count 0])
                            
                            (let* ([newstate (foldl (lambda (var value state) (state-update state var value))
                                                    state untyped-vars (to-eb-values next-enum))]
                                   [result (eval-predicate/quantified bare-pred newstate)])
                              
                              (handle-msgs)
                              
                              (cond [(thread? result) 
                                     ;; If it is a thread, we better wait
                                     (let loop ()
                                       (if (thread-running? result)
                                           (begin
                                             (sleep 2)
                                             (handle-msgs)
                                             (loop))
                                           ; thread is not running
                                           ; - handle result message
                                           ; and either continue enumeration
                                           ; or return
                                           ))
                                           
                                     ]
                                    [(ebtrue? result)
                                     (loop (enum) (enum 'prt) (+ count 1))]
                                    [else
                                     ;; Result is sent with the following shape:
                                     ;; (cons (list <id> 'result) (list <cover> <enumeration> <value>))
                                     (thread-send main-thread 
                                                  (cons (list thread-id 'result)
                                                        (list count next-enum
                                                              (if neg? ebtrue ebfalse))))])))))))))))))

;; By now any quantifiers are universal and are all
;; at the top of the predicate.
(define quantified-predicate? 
  (match-lambda
    [(or (struct Quantifier ('forall _ _))
         (struct Predicate-UnOp ('not (struct Quantifier ('forall _ _)))))
     #t]
    [_ #f]))
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
     ;(printf "eval-expression ~a, state ~a~n~n" expr state)
     (eval-ast expr state)]))

(define eval-predicate 
  (case-lambda
    [(expr state1 state2 . states)
     (eval-predicate expr (apply state-merge state1 state2 states))]
    [(expr state)
     ;(printf "eval-predicate ~a~nstate ~a~n" expr state)
     (let* ([val (eval-ast expr state)]
            [result (match val
                      [(struct Predicate-Literal ('btrue)) #t]
                      [_ #f])])
       ;(printf "=> ~a~n" result)
       result)]))

;;Transforms a list returned by a type enumerator into 
;; structures understandable by the eventb lib that can be evaluated.
(provide to-eb-values)
(define (to-eb-values lst)
  (letrec ([toval (lambda (elt)
                    (cond [(number? elt) (make-Integer-Literal elt)]
                          [(symbol? elt) (make-Set-Literal elt)]
                          [(null? elt) (make-Expression-Literal 'emptyset)]
                          [(list? elt) (make-Set-Enumeration (map toval elt))]
                          [(pair? elt) (make-Expression-BinOp 'mapsto (toval (car elt)) (toval (cdr elt)))]
                          [else (error 'to-eb-values "unexpected value in list: ~a" elt)]))])
    
    (map toval lst)))
