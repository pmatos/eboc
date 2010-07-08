#lang scheme/base

(require scheme/serialize
         scheme/contract
         scheme/match
         scheme/list
         (only-in srfi/1 find every lset-union lset-difference)
         (only-in srfi/13 string-drop-right)
         "../params.scm"
         "../utils.scm"
         "../types.scm"
         "../printer-tables.scm")


;                                                                               
;                                                                               
;                                                      ;                        
;   ;;;;;;                                                                      
;   ;                                                                           
;   ;      ;   ;  ;;;;    ; ;;   ;;;    ;;;    ;;;   ;;;     ;;;   ; ;;    ;;;  
;   ;       ; ;   ;; ;;   ;;  ; ;;  ;  ;   ;  ;   ;    ;    ;; ;;  ;;  ;  ;   ; 
;   ;;;;;;  ;;;   ;   ;   ;     ;   ;  ;      ;        ;    ;   ;  ;   ;  ;     
;   ;        ;    ;   ;   ;     ;;;;;   ;;;    ;;;     ;    ;   ;  ;   ;   ;;;  
;   ;       ; ;   ;   ;   ;     ;          ;      ;    ;    ;   ;  ;   ;      ; 
;   ;       ; ;   ;;  ;   ;     ;;  ;  ;   ;  ;   ;    ;    ;; ;;  ;   ;  ;   ; 
;   ;;;;;; ;   ;  ;;;;    ;      ;;;    ;;;    ;;;   ;;;;;   ;;;   ;   ;   ;;;  
;                 ;                                                             
;                 ;                                                             
;                 ;                                                             


(provide e
         subst-in-expression
         type->expression
         (struct-out Integer-Literal)
         (struct-out Lambda-Expression)
         (struct-out Set-Comprehension))

(define e 
  (match-lambda 
    ((? integer? int) 
     (make-Integer-Literal int))
    ((? (lambda (s) (memq s (available-ops expression-literal-table))) lit)
     (make-Expression-Literal lit))
    (`(,(? (lambda (s) (memq s (available-ops expression-unop-table))) op) ,arg)
     (make-Expression-UnOp op (e arg)))
    (`(,arg1 ,(? (lambda (s) (memq s (available-ops expression-binop-table))) op) ,arg2)
     (make-Expression-BinOp op (e arg1) (e arg2)))
    ((? symbol? name)
     (make-Variable name))
    ((? expression? e) e)
    (sexpr (error 'e "Cannot generate expression from s-expr: ~a"  sexpr))))

(define free-ids/expression 
  (match-lambda 
    ((or (struct Expression-Literal _) (struct Integer-Literal _)) '())
    ((? Identifier? id) (list id))
    ((struct Expression-Bool (pred)) (free-ids/predicate pred))
    ((struct Expression-UnOp (_ arg)) (free-ids/expression arg))
    ((struct Expression-BinOp (_ arg1 arg2)) 
     (lset-union variable/typed=? (free-ids/expression arg1) (free-ids/expression arg2)))
    ((struct Lambda-Expression (id-pat pred expr))
     (lset-difference variable/typed=? 
                      (lset-union variable/typed=? (free-ids/predicate pred) (free-ids/expression expr))
                      (free-ids/expression id-pat)))
    ((struct Set-Comprehension (comp-op vars pred expr))
     (lset-difference variable/typed=?
                      (lset-union variable/typed=? (free-ids/predicate pred) (free-ids/expression expr))
                      vars))
    ((struct Set-Enumeration (exprs))
     (apply lset-union variable/typed=? (map free-ids/expression exprs)))
    ((struct Variable-Pair (car cdr))
     (lset-union variable/typed=? (free-ids/expression car) (free-ids/expression cdr)))))

(define (free-vars/expression e)
  (filter Variable? (free-ids/expression e)))
(define (free-consts/expression e)
  (filter Constant? (free-ids/expression e)))
(define (free-set-literals/expression e)
  (filter Set-Literal? (free-ids/expression e)))
(define (free-sets/expression e)
  (filter Set? (free-ids/expression e)))

;; subst is an assoc of (variable . obj)
(define (subst-in-expression expr subst)
  (match expr
    ((or (struct Expression-Literal _)
         (struct Integer-Literal _)
         (struct Constant _)
         (struct Set _)
         (struct Set-Literal _ ))
     expr)
    ((struct Variable _)
     (let ([obj (assf (lambda (var) (variable=? expr var)) subst)])
       (if obj
           obj
           expr)))
    ((struct Expression-Bool (pred))
     (make-Expression-Bool (subst-in-predicate pred subst)))
    ((struct Expression-UnOp (op arg))
     (make-Expression-UnOp op (subst-in-expression arg subst)))
    ((struct Expression-BinOp (op arg1 arg2))
     (make-Expression-BinOp op (subst-in-expression arg1 subst) (subst-in-expression arg2 subst)))
    ((struct Lambda-Expression (id-pat pred expr))
     (make-Lambda-Expression (subst-in-expression id-pat subst)
                             (subst-in-predicate pred subst)
                             (subst-in-expression expr subst)))
    ((struct Set-Comprehension (op vars pred expr))
     (make-Set-Comprehension op
                             (map (lambda (v) (subst-in-expression v subst)) vars)
                             (subst-in-predicate pred subst)
                             (subst-in-expression expr subst)))
    ((struct Set-Enumeration (vals))
     (make-Set-Enumeration (map (lambda (e) (subst-in-expression e subst)) vals)))
    ((struct Variable-Pair (car cdr))
     (make-Variable-Pair (subst-in-expression car subst) (subst-in-expression cdr subst)))))

(define (type->expression type)
  (match type 
    ((struct Type-Integer _)
     (make-Expression-Literal 'integer))
    ((struct Type-Boolean _)
     (make-Expression-Literal 'bool))
    ((struct Type-Enumeration (name vals))
     (make-Set-Enumeration vals))
    ((struct Type-Powerset (subtype))
     (e `(pow ,(type->expression subtype))))
    ((struct Type-CartesianProduct (subtype1 subtype2))
     (e `(,(type->expression subtype1) cprod ,(type->expression subtype2))))))

(define (type-expression? expr)
  (match expr
    ((struct Expression-Literal ((or 'integer 'bool))) #t)
    ((struct Expression-UnOp ('pow arg))
     (type-expression? arg))
    ((struct Expression-BinOp ('cprod arg1 arg2))
     (and (type-expression? arg1) (type-expression? arg2)))
    (else #f)))

(define-serializable-struct Expression-Literal
  (val)
  #:guard 
  (lambda (val type-name)
    (values val))
  #:property prop:custom-write
  (lambda (struct port write?)
    (pp-Expression-Literal struct port)))

(define (pp-Expression-Literal struct  (port (current-output-port)))
  (print-table-constant expression-literal-table (Expression-Literal-val struct) port))

(define-serializable-struct Expression-Bool
  (pred)
  #:guard 
  (lambda (pred type-name)
    (values pred))
  #:property prop:custom-write
  (lambda (struct port write?)
    (pp-Expression-Bool struct port)))

(define (pp-Expression-Bool struct (port (current-output-port)))
  (fprintf port "bool(~a)" (Expression-Bool-pred struct)))

(define-serializable-struct Integer-Literal
  (val)
  #:property prop:custom-write
  (lambda (struct port write?)
    (pp-Integer-Literal struct port)))

(define (pp-Integer-Literal struct (port (current-output-port)))
  (fprintf port "~a" (Integer-Literal-val struct)))

(define-serializable-struct Expression-UnOp
  (op arg)
  #:property prop:custom-write
  (lambda (struct port write?)
    (pp-Expression-UnOp struct port)))

(define (pp-Expression-UnOp struct (port (current-output-port)))
  (print-table-operation expression-unop-table 
                         (Expression-UnOp-op struct)
                         (Expression-UnOp-arg struct)
                         port))

(define-serializable-struct Expression-BinOp
  (op arg1 arg2)
  #:property prop:custom-write
  (lambda (struct port write?)
    (pp-Expression-BinOp struct port)))

(define (pp-Expression-BinOp struct (port (current-output-port)))
  (print-table-operation expression-binop-table 
                         (Expression-BinOp-op struct)
                         (Expression-BinOp-arg1 struct)
                         (Expression-BinOp-arg2 struct)
                         port))

(define-serializable-struct Lambda-Expression
  (id-pattern ;: Variable-Pattern)
   pred ;: Predicate)
   expr ;: Expression)))
   ))

(define (pp-Lambda-Expression struct (port (current-output-port)))
  (fprintf port "<LAMBDA-EXPRESSION>"))

; Set-Comprehension-Op-Type (U 'none 'union 'inter))

(define-serializable-struct Set-Comprehension
  (comp-op ; Set-Comprehension-Op-Type)
   vars ; (Listof Typed-Variable))
   pred ; Predicate)
   expr ; Expression)))
   ))

(define (pp-Set-Comprehension struct (port (current-output-port)))
  (fprintf port "<SET-COMPREHENSION>"))

(define-serializable-struct Set-Enumeration
  (exprs)
  #:property prop:custom-write
  (lambda (struct port write?)
    (pp-Set-Enumeration struct port)))

(define (pp-Set-Enumeration struct (port (current-output-port)))
  (parameterize ([current-output-port port])
    (printf "{")
    (printf "~a" 
            (apply string-append 
                   (add-between
                    (map (lambda (e) 
                           (let ([o (open-output-string)])
                             (pp-expression e o)
                             (get-output-string o)))
                         (Set-Enumeration-exprs struct))
                    ", ")))
    (printf "}")))

(define-serializable-struct Variable-Pair
  (car cdr)
  #:property prop:custom-write
  (lambda (struct port write?)
    (pp-Variable-Pair struct port)))

(define (pp-Variable-Pair struct (port (current-output-port)))
  (fprintf port "<UNKNOWN>"))

(provide pp-expression)
(define (pp-expression struct (port (current-output-port)))
  (let ([pp-table `((,Expression-Literal?   . ,pp-Expression-Literal)
                    (,Expression-UnOp?      . ,pp-Expression-UnOp)
                    (,Expression-BinOp?     . ,pp-Expression-BinOp)
                    (,Set-Comprehension?    . ,pp-Set-Comprehension)
                    (,Set-Enumeration?      . ,pp-Set-Enumeration)
                    (,Variable-Pair?        . ,pp-Variable-Pair)
                    (,Lambda-Expression?    . ,pp-Lambda-Expression)
                    (,Variable?             . ,pp-Variable)
                    (,Set-Literal?          . ,pp-Set-Literal)
                    (,Set?                  . ,pp-Set)
                    (,Identifier?           . ,pp-Identifier)
                    (,Constant?             . ,pp-Constant)
                    (,Expression-Bool?      . ,pp-Expression-Bool)
                    (,Integer-Literal?      . ,pp-Integer-Literal)
                    (,Expr/wt?              . ,pp-Expr/wt))])
    (let loop ([rest-table pp-table])
      (cond [(null? rest-table)
             (error 'pp-expression "Expected an expression but got something else: ~a."  struct)]
            [((caar rest-table) struct)
             ((cdar rest-table) struct port)]
            [else
             (loop (rest rest-table))]))))


;                                                                                                           
;                                                                                                           
;                                                                                                           
;  ;;;;;;;                             ;;;;          ;;;                  ;   ;   ;;      ;    ;;;          
;     ;                                ;  ;;           ;                  ;   ;   ;;             ;          
;     ;    ;   ;; ;;;;    ;;;          ;   ;   ;;;     ;                  ;   ;  ;;;;;   ;;      ;     ;;;  
;     ;    ;   ;  ;;  ;  ;;  ;         ;   ;  ;;  ;    ;                  ;   ;   ;;      ;      ;    ;;    
;     ;    ;;  ;  ;   ;  ;   ;         ;;;;   ;   ;    ;                  ;   ;   ;;      ;      ;    ;;    
;     ;     ; ;   ;   ;  ;;;;;         ;  ;   ;;;;;    ;                  ;   ;   ;;      ;      ;     ;;;  
;     ;     ; ;   ;   ;  ;             ;   ;  ;        ;                  ;   ;    ;      ;      ;        ; 
;     ;      ;;   ;;  ;  ;             ;   ;  ;        ;      ;           ;   ;    ;      ;      ;        ; 
;     ;      ;    ;;;;    ;;;;         ;    ;  ;;;;    ;;;    ;            ;;;     ;;;  ;;;;;    ;;;  ;;;;  
;            ;    ;                                                                                         
;            ;    ;                                                                                         
;          ;;     ;                                                                                         

(define-struct Expr/wt
  (type expr)
  #:property prop:custom-write
  (lambda (struct port write?)
    (pp-Expr/wt struct port)))

(define (pp-Expr/wt struct (port (current-output-port)))
  (pp-expression (Expr/wt-expr struct) port)
  (when (print-debug?)
    (fprintf port "@|~a|" (Expr/wt-type struct))))

(define (typed-identifier? u)
  (and (Expr/wt? u)
       (Identifier? (Expr/wt-expr u))))

(define (typed-variable? u)
  (and (Expr/wt? u)
       (Variable? (Expr/wt-expr u))))

(define (typed-constant? u)
  (and (Expr/wt? u)
       (Constant? (Expr/wt-expr u))))

(define (typed-set? u)
  (and (Expr/wt? u)
       (Set? (Expr/wt-expr u))))

(define (typed-set-literal? u)
  (and (Expr/wt? u)
       (Set-Literal? (Expr/wt-expr u))))

;; Are all subexpressions are expressions with types
(define (typed-expression? e)
  (and (Expr/wt? e)
       (match (Expr/wt-expr e)
         ((or (struct Expression-Literal _) 
              (struct Integer-Literal _)
              (struct Set _)
              (struct Variable _)
              (struct Constant _)
              (struct Set-Literal _))
          #t)
         ((struct Expression-Bool (pred)) (typed-predicate? pred))
         ((struct Expression-UnOp (_ arg)) (typed-expression? arg))
         ((struct Expression-BinOp (_ arg1 arg2)) 
          (and (typed-expression? arg1) (typed-expression? arg2)))
         ((struct Lambda-Expression (id-pat pred expr))
          (and (typed-expression? id-pat) 
               (typed-predicate? pred) 
               (typed-expression? expr)))
         ((struct Set-Comprehension (comp-op vars pred expr))
          (and (andmap typed-variable? vars)
               (typed-predicate? pred)
               (typed-expression? expr)))
         ((struct Set-Enumeration (exprs))
          (andmap typed-expression? exprs))
         ((struct Variable-Pair (car cdr))
          (and (typed-expression? car) (typed-expression? cdr))))))

(define (typed-predicate? p)
  (match p
    ((struct Predicate-Literal _) #t)
    ((struct Predicate-UnOp (_ arg)) 
     (typed-predicate? arg))
    ((struct Predicate-BinOp (_ arg1 arg2))
     (and (typed-predicate? arg1) (typed-predicate? arg2)))
    ((struct Predicate-RelOp (_ arg1 arg2))
     (and (typed-expression? arg1) (typed-expression? arg2)))
    ((struct Quantifier (quant var body))
     (and (typed-variable? var)
          (typed-predicate? body)))))

(define (expression? u)
  (or (expr/wot? u)
      (Expr/wt? u)))

(define (expr/wot? u)
  (let ([preds (list Expression-Literal?
                     Integer-Literal?
                     Variable?
                     Set-Literal?
                     Set?
                     Constant?
                     Identifier?
                     Expression-Bool?
                     Expression-UnOp?
                     Expression-BinOp?
                     Lambda-Expression?
                     Set-Comprehension?
                     Set-Enumeration?
                     Variable-Pair?)])
    (anyof? preds u)))

(define (expression/wot= e1 e2)
  (if
   (match e1
     [(struct Expression-Literal (s))
      (and (Expression-Literal? e2)
           (eqv? s (Expression-Literal-val e2)))]
     
     [(struct Integer-Literal (num))
      (and (Integer-Literal? e2)
           (= num (Integer-Literal-val e2)))]
     
     [(struct Variable (name))
      (and (Variable? e2)
           (eqv? name (Variable-name e2)))]
     
     [(struct Set-Literal (name))
      (and (Set-Literal? e2)
           (eqv? name (Set-Literal-name e2)))]
     
     [(struct Set (name))
      (and (Set? e2)
           (eqv? name (Set-name e2)))]
     
     [(struct Constant (name))
      (and (Constant? e2)
           (eqv? name (Constant-name e2)))]
     
     [(struct Identifier (name))
      (and (Identifier? e2)
           (eqv? name (Identifier-name e2)))]
     
     [(struct Expression-Bool (pred))
      (and (Expression-Bool? e2)
           (predicate= pred (Expression-Bool-pred e2)))]
     
     [(struct Expression-UnOp (op arg))
      (and (Expression-UnOp? e2)
           (eqv? op (Expression-UnOp-op e2))
           (expression/wot= arg (Expression-UnOp-arg e2)))]
     
     [(struct Expression-BinOp (op arg1 arg2))
      (and (Expression-BinOp? e2)
           (eqv? op (Expression-BinOp-op e2))
           (expression/wot= arg1 (Expression-BinOp-arg1 e2))
           (expression/wot= arg2 (Expression-BinOp-arg2 e2)))]
     
     [(struct Set-Enumeration (exprs))
      (and (Set-Enumeration? e2)
           (andmap (lambda (expr) (find (lambda (e2-el) (expression/wot= expr e2-el))
                                        (Set-Enumeration-exprs e2)))
                   exprs))]
     
     [_
      (error 'expression/wot= 
             "Unimplemented equality between expressions: ~a, ~a" e1 e2)])
   #t #f))




;                                                                               
;                                                                               
;              ;                         ;       ;;    ;                        
;   ;;;;;      ;                  ;             ;                               
;     ;        ;                  ;             ;                               
;     ;     ;;;;   ;;;   ;;;;   ;;;;;   ;;    ;;;;;   ;;     ;;;    ; ;;;  ;;;  
;     ;    ;  ;;  ;;  ;  ;;  ;    ;      ;      ;      ;    ;;  ;   ;;    ;;    
;     ;    ;   ;  ;   ;  ;   ;    ;      ;      ;      ;    ;   ;   ;     ;;    
;     ;    ;   ;  ;;;;;  ;   ;    ;      ;      ;      ;    ;;;;;   ;      ;;;  
;     ;    ;   ;  ;      ;   ;    ;      ;      ;      ;    ;       ;         ; 
;     ;    ;   ;  ;      ;   ;    ;      ;      ;      ;    ;       ;         ; 
;   ;;;;;   ;;;;   ;;;;  ;   ;    ;;;  ;;;;;    ;    ;;;;;   ;;;;   ;     ;;;;  
;                                                                               
;                                                                               
;
(define-serializable-struct Identifier
  (name)
  #:property prop:custom-write
  (lambda (struct port write?)
    (pp-Identifier struct port)))

(define (pp-Identifier struct (out (current-output-port)))
  (fprintf out "~a" 
           (if (print-debug?)
               (symbol-append 'id: (Identifier-name struct))
               (Identifier-name struct))))

(define (identifier=? id1 id2)
  (eq? (Identifier-name id1) (Identifier-name id2)))

(define (identifier/typed=? i1 i2)
  (let ([id1 (if (Expr/wt? i1) (Expr/wt-expr i1) i1)]
        [id2 (if (Expr/wt? i2) (Expr/wt-expr i2) i2)])
    (identifier=? id1 id2)))

;                                                                        
;                                                                        
;                            ;    ;                                      
;   ;;;;;                    ;                         ;                 
;   ;    ;                   ;                         ;                 
;   ;    ;  ; ;;   ;;;    ;;;;  ;;;     ;;;    ;;;   ;;;;;   ;;;    ;;;  
;   ;    ;  ;;  ; ;;  ;  ;; ;;    ;    ;;  ;  ;   ;    ;    ;;  ;  ;   ; 
;   ;;;;;   ;     ;   ;  ;   ;    ;    ;          ;    ;    ;   ;  ;     
;   ;       ;     ;;;;;  ;   ;    ;    ;       ;;;;    ;    ;;;;;   ;;;  
;   ;       ;     ;      ;   ;    ;    ;      ;   ;    ;    ;          ; 
;   ;       ;     ;;  ;  ;  ;;    ;    ;;  ;  ;   ;    ;    ;;  ;  ;   ; 
;   ;       ;      ;;;    ;;;;  ;;;;;   ;;;    ;; ;    ;;;   ;;;    ;;;  
;                                                                        
;                                                                        
;                                                                        

(provide predicate?
         p
         subst-in-predicate
         splice/predicate
         join/predicate
         (struct-out Predicate-UnOp)
         (struct-out Predicate-BinOp))

(define p
  (match-lambda 
    ((? (lambda (s) (memq s (available-ops predicate-literal-table))) lit)
     (make-Predicate-Literal lit))
    (`(,(? (lambda (s) (memq s (available-ops predicate-unop-table))) op) ,arg)
     (make-Predicate-UnOp op (p arg)))
    (`(,arg1 ,(? (lambda (s) (memq s (available-ops predicate-binop-table))) op) ,arg2)
     (make-Predicate-BinOp op (p arg1) (p arg2)))
    (`(,arg1 ,(? (lambda (s) (memq s (available-ops predicate-relop-table))) op) ,arg2)
     (make-Predicate-RelOp op (e arg1) (e arg2)))
    ((? predicate? p) p)
    (sexpr (error 'p "Cannot generate predicate from s-expr: ~a"  sexpr))))

(define free-ids/predicate
  (match-lambda
    ((struct Predicate-Literal _) '())
    ((struct Predicate-UnOp (_ arg)) (free-ids/predicate arg))
    ((struct Predicate-BinOp (_ arg1 arg2)) 
     (lset-union variable/typed=? (free-ids/predicate arg1) (free-ids/predicate arg2)))
    ((struct Predicate-RelOp (_ arg1 arg2))
     (lset-union variable/typed=? (free-ids/expression arg1) (free-ids/expression arg2)))
    ((struct Quantifier (quant var body))
     (lset-difference variable/typed=? (free-ids/predicate body) (list var)))))

(define (free-vars/predicate p)
  (filter Variable? (free-ids/predicate p)))

(define (splice/predicate p (splice-op 'land))
  (match p
    ((struct Predicate-BinOp (op arg1 arg2))
     (if (eq? splice-op op)
         (append (splice/predicate arg1 splice-op)
                 (splice/predicate arg2 splice-op))
         (list p)))
    (_ (list p))))

(define (join/predicate #:op (join-op 'land) pred . preds)
  (foldl (lambda (el pred) (p `(,pred ,join-op ,el))) pred preds))

(define (predicate? u)
  (anyof? (list Predicate-Literal? Predicate-UnOp? Predicate-BinOp? Predicate-RelOp? Quantifier? Predicate-Finite? Predicate-Partition?) u))

(define (predicate= p1 p2)
  (match p1
    [(struct Predicate-Literal (lit))
     (and (Predicate-Literal? p2)
          (eqv? lit (Predicate-Literal-lit p2)))]
    
    [(struct Predicate-UnOp (op arg))
     (and (Predicate-UnOp? p2)
          (eqv? op (Predicate-UnOp-op p2))
          (predicate= arg (Predicate-UnOp-arg p2)))]
    
    [(struct Predicate-BinOp (op arg1 arg2))
     (and (Predicate-BinOp? p2)
          (eqv? op (Predicate-BinOp-op p2))
          (predicate= arg1 (Predicate-BinOp-arg1 p2))
          (predicate= arg2 (Predicate-BinOp-arg2 p2)))]
    
    [(struct Predicate-RelOp (op arg1 arg2))
     (and (Predicate-RelOp? p2)
          (eqv? op (Predicate-RelOp-op arg2))
          (expression/wot= arg1 (Predicate-RelOp-arg1 p2))
          (expression/wot= arg2 (Predicate-RelOp-arg2 p2)))]
    
    [(struct Quantifier (quant var body))
     (and (Quantifier? p2)
          (eqv? quant (Quantifier-quant p2))
          (variable=? var (Quantifier-var p2))
          (predicate= body (Quantifier-body p2)))]
    
    [(struct Predicate-Finite (expr))
     (and (Predicate-Finite? p2)
          (expression/wot= expr (Predicate-Finite-expr p2)))]
    
    [_
     (error 'predicate=
            "Unimplemented equality between predicates ~a, ~a" p1 p2)]))

(define-serializable-struct Predicate-Literal
  (lit)
  #:property prop:custom-write
  (lambda (struct port write?)
    (pp-Predicate-Literal struct port)))

(define (pp-Predicate-Literal struct (port (current-output-port)))
  (print-table-constant predicate-literal-table (Predicate-Literal-lit struct) port))

;; Recursively substitutes certain variables for something else
(define (subst-in-predicate pred subst)
  (match pred
    ((struct Predicate-Literal _) pred)
    ((struct Predicate-UnOp (op arg)) 
     (make-Predicate-UnOp op (subst-in-predicate arg subst)))
    ((struct Predicate-BinOp (op arg1 arg2))
     (make-Predicate-BinOp op (subst-in-predicate arg1 subst) (subst-in-predicate arg2 subst)))
    ((struct Predicate-RelOp (op arg1 arg2))
     (make-Predicate-RelOp op (subst-in-expression arg1 subst) (subst-in-expression arg2 subst)))
    ((struct Quantifier (quant var body))
     (make-Quantifier quant (subst-in-predicate var subst) (subst-in-predicate body subst)))))


(define-serializable-struct Predicate-UnOp
  (op arg)
  #:guard 
  (lambda (op arg type-name)
    (unless (and (memq op (available-ops predicate-unop-table))
                 (predicate? arg))
      (error 'Predicate-UnOp:guard "Arguments for Predicate-UnOp are wrong."))
    (values op arg))
  #:property prop:custom-write
  (lambda (struct port write?)
    (pp-Predicate-UnOp struct port)))

(define (pp-Predicate-UnOp struct (port (current-output-port)))
  (print-table-operation predicate-unop-table 
                         (Predicate-UnOp-op struct)
                         (Predicate-UnOp-arg struct)
                         port))

(define-serializable-struct Predicate-BinOp
  (op arg1 arg2)
  #:guard 
  (lambda (op arg1 arg2 type-name)
    (unless (and (memv op (available-ops predicate-binop-table))
                 (predicate? arg1)
                 (predicate? arg2))
      (error 'Predicate-BinOp:guard "Arguments for guard: op: ~a, args: (~a, ~a)" op arg1 arg2))
    (values op arg1 arg2))
  #:property prop:custom-write
  (lambda (struct port write?)
    (pp-Predicate-BinOp struct port)))

(define (pp-Predicate-BinOp struct (port (current-output-port)))
  (print-table-operation predicate-binop-table 
                         (Predicate-BinOp-op struct)
                         (Predicate-BinOp-arg1 struct)
                         (Predicate-BinOp-arg2 struct)
                         port))

(define-serializable-struct Predicate-RelOp
  (op arg1 arg2)
  #:property prop:custom-write
  (lambda (struct port write?)
    (pp-Predicate-RelOp struct port)))

(define (pp-Predicate-RelOp struct (port (current-output-port)))
  (print-table-operation predicate-relop-table 
                         (Predicate-RelOp-op struct)
                         (Predicate-RelOp-arg1 struct)
                         (Predicate-RelOp-arg2 struct)
                         port))

(define-serializable-struct Predicate-Partition
  (args)
  #:property prop:custom-write
  (lambda (struct port write?)
    (pp-Predicate-Partition struct port)))

(define (pp-Predicate-Partition struct (port (current-output-port)))
  (fprintf port "partition(")
  (let loop ([rest-args (Predicate-Partition-args struct)])
    (cond [(null? rest-args)
           (fprintf port ")")]
          [(null? (rest rest-args))
           (fprintf port "~a)" (first rest-args))]
          [else
           (fprintf port "~a, " (first rest-args))
           (loop (rest rest-args))])))

(define-serializable-struct Predicate-Finite
  (expr)
  #:property prop:custom-write
  (lambda (struct port write?)
    (pp-Predicate-Finite struct port)))

(define (pp-Predicate-Finite struct (port (current-output-port)))
  (fprintf port "finite(~a)" (Predicate-Finite-expr struct)))

(define-serializable-struct Quantifier
  (quant var body)
  #:property prop:custom-write
  (lambda (struct port write?)
    (pp-Quantifier struct port)))

(define (pp-Quantifier struct (port (current-output-port)))
  (print-table-constant quantifier-op-table (Quantifier-quant struct) port)
  (fprintf port " ~a " (Quantifier-var struct))
  (print-table-constant constant-characters-table 'qdot port)
  (fprintf port " ~a" (Quantifier-body struct)))

;                                                                                      
;                                                                                      
;                                        ;                                ;;;          
;    ;;;;           ;           ;               ;                           ;          
;   ;               ;           ;               ;                           ;          
;   ;       ;;;   ;;;;;         ;       ;;    ;;;;;   ;;;    ; ;;;  ;;;     ;     ;;;  
;   ;;     ;;  ;    ;           ;        ;      ;    ;;  ;   ;;        ;    ;    ;;    
;    ;;;   ;   ;    ;           ;        ;      ;    ;   ;   ;         ;    ;    ;;    
;       ;  ;;;;;    ;           ;        ;      ;    ;;;;;   ;      ;;;;    ;     ;;;  
;       ;  ;        ;           ;        ;      ;    ;       ;     ;   ;    ;        ; 
;       ;  ;        ;           ;        ;      ;    ;       ;     ;   ;    ;        ; 
;   ;;;;    ;;;;    ;;;         ;;;;;; ;;;;;    ;;;   ;;;;   ;     ;;;;;    ;;;  ;;;;  
;                                                                                      
;                                                                                      
;                                                                                  ; ;;
;; Enumerated sets are composed of set literals : uninterpreted symbols
(define-serializable-struct (Set-Literal Identifier)
  ()
  #:property prop:custom-write
  (lambda (struct port write?)
    (pp-Set-Literal struct port)))

(define (pp-Set-Literal struct (port (current-output-port)))
  (fprintf port "~a" 
           (if (print-debug?)
               (symbol-append 'sl: (Identifier-name struct))
               (Identifier-name struct))))

(define Set-Literal-name Identifier-name)
(define set-literal=? identifier=?)

(define (set-literal/typed=? sl1 sl2)
  (let ([slit1 (if (Expr/wt? sl1) (Expr/wt-expr sl1) sl1)]
        [slit2 (if (Expr/wt? sl2) (Expr/wt-expr sl2) sl2)])
    (set-literal=? slit1 slit2)))

;                                                                 
;                                                                 
;                                                                 
;     ;;;                         ;                    ;          
;    ;                            ;                    ;          
;   ;       ;;;   ;;;;    ;;;   ;;;;;   ;;;   ;;;;   ;;;;;   ;;;  
;   ;      ;   ;  ;;  ;  ;;       ;        ;  ;;  ;    ;    ;;    
;   ;      ;   ;  ;   ;  ;;       ;        ;  ;   ;    ;    ;;    
;   ;      ;   ;  ;   ;   ;;;     ;     ;;;;  ;   ;    ;     ;;;  
;   ;      ;   ;  ;   ;      ;    ;    ;   ;  ;   ;    ;        ; 
;    ;     ;   ;  ;   ;      ;    ;    ;   ;  ;   ;    ;        ; 
;     ;;;   ;;;   ;   ;  ;;;;     ;;;  ;;;;;  ;   ;    ;;;  ;;;;  
;                                                                 
;                                                                 
;                                                                 

(define-serializable-struct (Constant Identifier)
  ()
  #:property prop:custom-write
  (lambda (struct port write?)
    (pp-Constant struct port)))

(define (pp-Constant struct (port (current-output-port)))
  (fprintf port "~a" 
           (if (print-debug?)
               (symbol-append 'const: (Constant-name struct))
               (Set-name struct))))

(define Constant-name Identifier-name)
(define constant=? identifier=?)

(define (constant/typed=? c1 c2)
  (let ([const1 (if (Expr/wt? c1) (Expr/wt-expr c1) c1)]
        [const2 (if (Expr/wt? c2) (Expr/wt-expr c2) c2)])
    (constant=? const1 const2)))

;                              
;                              
;                              
;    ;;;;           ;          
;   ;;   ;          ;          
;   ;       ;;;   ;;;;;   ;;;  
;   ;;     ;;  ;    ;    ;   ; 
;    ;;;;  ;   ;    ;    ;     
;       ;; ;;;;;    ;     ;;;  
;        ; ;        ;        ; 
;   ;    ; ;;  ;    ;    ;   ; 
;    ;;;;   ;;;     ;;;   ;;;  
;                              
;                              
;                              

(define-serializable-struct (Set Identifier)
  ()
  #:property prop:custom-write
  (lambda (struct port write?)
    (pp-Set struct port)))

(define (pp-Set struct (port (current-output-port)))
  (fprintf port "~a" 
           (if (print-debug?)
               (symbol-append 'set: (Set-name struct))
               (Set-name struct))))

(define Set-name Identifier-name)
(define set=? identifier=?)

(define (set/typed=? s1 s2)
  (let ([set1 (if (Expr/wt? s1) (Expr/wt-expr s1) s1)]
        [set2 (if (Expr/wt? s2) (Expr/wt-expr s2) s2)])
    (set=? set1 set2)))

;                                                                 
;                                                                 
;                          ;           ;      ;;;                 
;   ;    ;                             ;        ;                 
;   ;    ;                             ;        ;                 
;    ;  ;   ;;;    ; ;;  ;;;     ;;;   ;;;;     ;     ;;;    ;;;  
;    ;  ;  ;   ;   ;;  ;   ;    ;   ;  ;; ;;    ;    ;;  ;  ;   ; 
;    ;  ;      ;   ;       ;        ;  ;   ;    ;    ;   ;  ;     
;    ;  ;   ;;;;   ;       ;     ;;;;  ;   ;    ;    ;;;;;   ;;;  
;     ;;   ;   ;   ;       ;    ;   ;  ;   ;    ;    ;          ; 
;     ;;   ;   ;   ;       ;    ;   ;  ;;  ;    ;    ;;  ;  ;   ; 
;     ;;    ;; ;   ;     ;;;;;   ;; ;  ;;;;      ;;   ;;;    ;;;  
;                                                                 
;                                                                 
;                                                                 
(provide variable->post-variable
         post-variable->variable
         typed-variable->typed-post-variable
         pp-Variable)

(define-serializable-struct (Variable Identifier)
  ()
  #:property prop:custom-write
  (lambda (struct port write?)
    (pp-Variable struct port)))

(define (pp-Variable struct (port (current-output-port)))
  (if (print-debug?)
      (fprintf port "var:~a" (Variable-name struct))
      (fprintf port "~a" (Variable-name struct))))

(define (pp-typed-variable struct (port (current-output-port)))
  (fprintf port "~a" (Expr/wt-expr struct))
  (when (print-debug?)
    (fprintf port "@|~a|" (Expr/wt-type struct))))

(define (variable->post-variable var)
  (unless (Variable? var)
    (error 'variable->post-variable "Expected variable, got ~a" var))
  (make-Variable (symbol-append (Identifier-name var) '|'|)))

(define (post-variable->variable var)
  (make-Variable (string->symbol (string-drop-right (symbol->string (Identifier-name var)) 1))))

(define (typed-variable->typed-post-variable var)
  (make-Expr/wt (Expr/wt-type var)
                (variable->post-variable (Expr/wt-expr var))))

(define variable=? identifier=?)

(define (variable/typed=? v1 v2)
  (let ([var1 (if (Expr/wt? v1) (Expr/wt-expr v1) v1)]
        [var2 (if (Expr/wt? v2) (Expr/wt-expr v2) v2)])
    (variable=? var1 var2)))

(define Variable-name Identifier-name)


;                                                                               
;                                                                               
;                                                                               
;                          ;                                                    
;                                                                               
;           ;                                  ;                                
;    ;;;   ;;;;   ; ;;;   ;;    ;;;;          ;;;;   ;   ;  ;;;;    ;;;    ;;;  
;   ;       ;     ;;       ;    ;   ;  ;;;;;   ;     ;   ;  ;   ;  ;   ;  ;     
;    ;;     ;     ;        ;    ;   ;          ;      ; ;   ;   ;  ;;;;;   ;;   
;      ;;   ;     ;        ;    ;   ;          ;      ; ;   ;   ;  ;         ;; 
;       ;   ;     ;        ;    ;   ;          ;       ;    ;   ;  ;          ; 
;   ;;;;     ;;;  ;      ;;;;   ;;;;            ;;;    ;    ;;;;    ;;;   ;;;;  
;                               ;                      ;    ;                   
;                               ;                    ;;     ;                   
;                                                                               

;; Given an expression or a predicate, returns the same expression/predicate
;; but without any typing information
(provide strip-types)

(define (strip-types expr/pred)
  (letrec ([strip-types/pred 
            (lambda (pred)
              (match pred
                [(struct Predicate-Literal _) pred]
                [(struct Predicate-UnOp (op arg))
                 (make-Predicate-UnOp op (strip-types/pred arg))]
                [(struct Predicate-BinOp (op arg1 arg2))
                 (make-Predicate-BinOp op 
                                       (strip-types/pred arg1) 
                                       (strip-types/pred arg2))]
                [(struct Predicate-RelOp (op arg1 arg2))
                 (make-Predicate-RelOp op
                                       (strip-types/expr arg1)
                                       (strip-types/expr arg2))]
                [(struct Quantifier (quant var body))
                 (make-Quantifier quant
                                  (strip-types/expr var)
                                  (strip-types/pred body))]
                [(struct Predicate-Finite (expr))
                 (make-Predicate-Finite (strip-types/expr expr))]
                [(struct Predicate-Partition (args))
                 (make-Predicate-Partition (map strip-types/expr args))]
                [_
                 (error 'strip-types/pred
                        "Can't match predicate structure" pred)]))]
           
           [strip-types/expr
            (lambda (expr)
              (match expr
                [(struct Expr/wt (type expr))
                 (strip-types/expr expr)]
                [(or (struct Expression-Literal _)
                     (struct Integer-Literal _)
                     (struct Variable _) 
                     (struct Set-Literal _)
                     (struct Set _)
                     (struct Constant _)
                     (struct Identifier _))
                 expr]
                [(struct Expression-Bool (pred))
                 (make-Expression-Bool (strip-types/pred pred))]
                [(struct Expression-UnOp (op arg))
                 (make-Expression-UnOp op (strip-types/expr arg))]
                [(struct Expression-BinOp (op arg1 arg2))
                 (make-Expression-BinOp op 
                                        (strip-types/expr arg1)
                                        (strip-types/expr arg2))]
                [(struct Lambda-Expression (idpat pred expr))
                 (make-Lambda-Expression (strip-types/expr idpat)
                                         (strip-types/pred pred)
                                         (strip-types/expr expr expr))]
                [(struct Set-Comprehension (compop vars pred expr))
                 (make-Set-Comprehension compop
                                         (strip-types/expr vars)
                                         (strip-types/pred pred)
                                         (strip-types/expr expr))]
                [(struct Set-Enumeration (exprs))
                 (make-Set-Enumeration (map strip-types/expr exprs))]
                [(struct Variable-Pair (car cdr))
                 (make-Variable-Pair (strip-types/expr car)
                                     (strip-types/expr cdr))]
                [_
                 (error 'strip-types/expr
                        "Can't match expression structure" expr)]))])
    
    (if (predicate? expr/pred)
        (strip-types/pred expr/pred)
        (strip-types/expr expr/pred))))



;                                                                                                                  
;                                                                                                                  
;                                                                                                                  
;                                        ;;;         ;;;                                                  ;        
;                                   ;   ;              ;                                                  ;        
;    ;                             ;    ;              ;                                                  ;        
;   ;;;;   ; ;;;  ;   ;   ;;;      ;   ;;;;    ;;;     ;     ;;;    ;;;          ;;;;   ; ;;;   ;;;    ;;;;   ;;;  
;    ;     ;;     ;   ;  ;   ;    ;     ;         ;    ;    ;      ;   ;         ;   ;  ;;     ;   ;  ;   ;  ;     
;    ;     ;      ;   ;  ;;;;;    ;     ;      ;;;;    ;     ;;    ;;;;;         ;   ;  ;      ;;;;;  ;   ;   ;;   
;    ;     ;      ;   ;  ;       ;      ;     ;   ;    ;       ;;  ;             ;   ;  ;      ;      ;   ;     ;; 
;    ;     ;      ;  ;;  ;       ;      ;     ;  ;;    ;        ;  ;             ;   ;  ;      ;      ;  ;;      ; 
;     ;;;  ;       ;;;;   ;;;   ;       ;     ;;;;;  ;;;;   ;;;;    ;;;          ;;;;   ;       ;;;    ;;;;  ;;;;  
;                                                                                ;                                 
;                                                                                ;                                 
;                                                                                                                  
 
(define ebtrue (make-Predicate-Literal 'btrue))
(define ebfalse (make-Predicate-Literal 'bfalse))
(define (ebtrue? u)
  (and (Predicate-Literal? u)
       (eqv? (Predicate-Literal-lit u) 'btrue)))
(define (ebfalse? u)
  (and (Predicate-Literal? u)
       (eqv? (Predicate-Literal-lit u) 'bfalse)))
(define-syntax eband 
  (syntax-rules ()
    [(eband a ...) (and (ebtrue? a) ...)]))
(define-syntax ebor
  (syntax-rules ()
    [(ebor a ...) (or (ebtrue? a) ...)]))
(define ebnot ebfalse?)

(provide eband ebor)

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
 [struct Predicate-Literal ((lit (one-of/c 'btrue 'bfalse)))]
 [ebtrue ebtrue?]
 [ebfalse ebfalse?]
 [ebtrue? (any/c . -> . boolean?)]
 [ebfalse? (any/c . -> . boolean?)]
 [ebnot (Predicate-Literal? . -> . Predicate-Literal?)]
 [struct Predicate-Partition ((args (listof expression?)))]
 [struct Predicate-Finite ((expr expression?))]
 [struct Variable-Pair ((car (or/c Variable? Variable-Pair?)) (cdr (or/c Variable? Variable-Pair?)))]
 [struct Predicate-RelOp ((op (apply one-of/c (available-ops predicate-relop-table)))
                          (arg1 expression?)
                          (arg2 expression?))]
 [struct Quantifier ((quant (apply one-of/c (available-ops quantifier-op-table)))
                     (var (or/c Variable? typed-variable?))
                     (body predicate?))]
 [struct Expression-Literal ((val (apply one-of/c (available-ops expression-literal-table))))]
 [struct Expression-Bool ((pred predicate?))]
 [struct Expression-UnOp ((op (apply one-of/c (available-ops expression-unop-table)))
                          (arg expression?))]
 [struct Expression-BinOp ((op (apply one-of/c (available-ops expression-binop-table)))
                           (arg1 expression?)
                           (arg2 expression?))]
 [struct Set-Enumeration ((exprs (non-empty-listof expression?)))]
 [struct Expr/wt ((type type?) (expr expr/wot?))]
 [struct (Variable Identifier) ((name symbol?))]
 [Variable-name (Variable? . -> . symbol?)]
 [struct (Set Identifier) ((name symbol?))]
 [Set-name (Set? . -> . symbol?)]
 [struct (Set-Literal Identifier) ((name symbol?))]
 [Set-Literal-name (Set-Literal? . -> . symbol?)]
 [struct (Constant Identifier) ((name symbol?))]
 [Constant-name (Constant? . -> . symbol?)]
 [struct Identifier ((name symbol?))]
 [variable=? (Variable? Variable? . -> . boolean?)]
 [identifier=? (Identifier? Identifier? . -> . boolean?)]
 [constant=? (Constant? Constant? . -> . boolean?)]
 [set-literal=? (Set-Literal? Set-Literal? . -> . boolean?)]
 [set=? (Set? Set? . -> . boolean?)]
 [free-ids/predicate (predicate? . -> . (listof identifier?))]
 [free-ids/expression (expression? . -> . (listof identifier?))]
 [free-vars/predicate (predicate? . -> . (listof Variable?))]
 [free-vars/expression (expression? . -> . (listof Variable?))]
 [typed-expression? (any/c . -> . boolean?)]
 [predicate= (predicate? predicate? . -> . boolean?)]
 [typed-predicate? (any/c . -> . boolean?)]
 [typed-variable? (any/c . -> . boolean?)]
 [typed-identifier? (any/c . -> . boolean?)]
 [typed-constant? (any/c . -> . boolean?)]
 [typed-set? (any/c . -> . boolean?)]
 [typed-set-literal? (any/c . -> . boolean?)]
 [expression? (any/c . -> . boolean?)]
 [type-expression? (any/c . -> . boolean?)]
 [expr/wot? (any/c . -> . boolean?)]
 [expression/wot= (expr/wot? expr/wot? . -> . boolean?)]
 [identifier/typed=? ((or/c Identifier? typed-identifier?) (or/c Identifier? typed-identifier?) . -> . boolean?)]
 [variable/typed=? ((or/c Variable? typed-variable?) (or/c Variable? typed-variable?) . -> . boolean?)]
 [constant/typed=? ((or/c Constant? typed-constant?) (or/c Constant? typed-constant?) . -> . boolean?)]
 [set-literal/typed=? ((or/c Set-Literal? typed-set-literal?) (or/c Set-Literal? typed-set-literal?) . -> . boolean?)]
 [set/typed=? ((or/c Set? typed-set?) (or/c Set? typed-set?) . -> . boolean?)])
