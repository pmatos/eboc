#lang scheme

(require scheme/match
         (only-in srfi/1 find)
         "../../ast/predexpr.scm"
         "state.scm"
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

(define/contract (eval-ast ast state)
  ((or/c expr/wot? predicate?) state? . -> . (or/c expr/wot? predicate?))
  
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
            [_ (make-Predicate-Literal 'bfalse)])]

         [(lor) 
          (match (cons earg1 earg2)
            [(cons (struct Predicate-Literal ('bfalse))
                   (struct Predicate-Literal ('bfalse)))
             earg1]
            [_ (make-Predicate-Literal 'btrue)])]
         
         [(limp) 
          (match (cons earg1 earg2)
            [(cons (struct Predicate-Literal ('btrue))
                   (struct Predicate-Literal ('bfalse)))
             earg2]
            [_ (make-Predicate-Literal 'btrue)])]
         
         [(leqv) 
          (match (list earg1 earg2)
            [(list-no-order (struct Predicate-Literal ('bfalse))
                            (struct Predicate-Literal ('btrue)))
             (make-Predicate-Literal 'bfalse)]
            [_ (make-Predicate-Literal 'btrue)])]))]
             
    [(struct Predicate-RelOp ('notequal arg1 arg2))
     (eval-ast (make-Predicate-UnOp 'not (make-Predicate-RelOp 'equal arg1 arg2)) state)]
    
    [(struct Predicate-RelOp ('notin arg1 arg2))
     (eval-ast (make-Predicate-UnOp 'not (make-Predicate-RelOp 'in arg1 arg2)) state)]
    
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
           (make-Predicate-Literal 'btrue)
           (make-Predicate-Literal 'bfalse)))]
    
    [(struct Predicate-RelOp ('equal arg1 arg2)) 
     (let ([earg1 (eval-ast arg1 state)]
           [earg2 (eval-ast arg2 state)])
       
       (if (expression/wot= earg1 earg2)
           (make-Predicate-Literal 'btrue)
           (make-Predicate-Literal 'bfalse)))]
             
    [(struct Predicate-RelOp ('in arg1 arg2))
     
     (let ([earg1 (eval-ast arg1 state)]
           [earg2 (eval-ast arg2 state)])
           
       (match (cons earg1 earg2)
         [(cons _ (struct Expression-Literal ('emptyset)))
          (make-Predicate-Literal 'bfalse)]
         
         [(cons _ (struct Expression-Literal ('integer)))
          (make-Predicate-Literal 'btrue)]
         
         [(cons _ (struct Expression-Literal ('natural)))
          (if (>= (Integer-Literal-val earg1) 0)
              (make-Predicate-Literal 'btrue)
              (make-Predicate-Literal 'bfalse))]
         
         [(cons _ (struct Set-Enumeration (exprs)))
          (if (ormap (lambda (x) (expression/wot= earg1 x)) exprs)
              (make-Predicate-Literal 'btrue)
              (make-Predicate-Literal 'bfalse))]
         
         [(cons (struct Expression-Literal ('emptyset))
                (struct Expression-UnOp ('pow _)))
          (make-Predicate-Literal 'btrue)]
         
         [(cons (struct Set-Enumeration (exprs1))
                (struct Expression-UnOp ('pow (struct Set-Enumeration (exprs2)))))
          (if (andmap (lambda (expr1)
                        (find (lambda (expr2) (expression/wot= expr1 expr2))
                              exprs2))
                      exprs1)
              (make-Predicate-Literal 'btrue)
              (make-Predicate-Literal 'bfalse))]
         
         [else
          (error 'eval-ast/Rel-Op/in
                 "Received unexpected args to in: ~a, ~a" earg1 earg2)]))]
                
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
     (printf "eval-expression ~a, state ~a~n~n" expr state)
     (eval-ast expr state)]))

(define eval-predicate 
  (case-lambda
    [(expr state1 state2 . states)
     (eval-predicate expr (apply state-merge state1 state2 states))]
    [(expr state)
     (printf "eval-predicate ~a~nstate ~a~n" expr state)
     (let* ([val (eval-ast expr state)]
            [result (match val
                      [(struct Predicate-Literal ('btrue)) #t]
                      [_ #f])])
       (printf "=> ~a~n" result)
       result)]))

;;Transforms a list returned by a type enumerator into 
;; structures understandable by the eventb lib that can be evaluated.
(provide to-eb-values)
(define (to-eb-values lst)
  (map (lambda (elt)
         (cond [(number? elt) (make-Integer-Literal elt)]
               [(symbol? elt) (make-Set-Literal elt)]
               [(list? elt)
                (make-Set-Enumeration (map to-eb-values elt))]
               [else (error 'to-eb-values "unexpected value in list: ~a" elt)]))
       lst))
