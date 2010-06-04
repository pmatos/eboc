#lang scheme

(require scheme/match
         "../../ast/predexpr.scm")

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
  
  (match ast
    
    ;; Evaluation of Unary Expressions
   
    ; converse: POW(a x b) -> POW(b x a)
    [(struct Expression-UnOp ('converse arg))
     (error "Unimplemented.")]
    
    ; uminus: INT -> INT
    [(struct Expression-UnOp ('uminus arg))
     (make-Integer-Literal (- (Integer-Literal-val arg)))]
    
    ; card: POW(a) -> INT
    [(struct Expression-UnOp ('card arg))
     (error "Unimplemented.")]
    
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
     (error "Unimplemented.")]
    
    ; max: POW(INT) -> INT
    [(struct Expression-UnOp ('max arg))
     (error "Unimplemented.")]
    
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
     (error "Unimplemented.")]
    
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
     (make-Integer-Literal (mod (Integer-Literal-val arg1)
                                (Integer-Literal-val arg2)))]
    
    ; expn: INT x INT -> INT
    [(struct Expression-BinOp ('expn arg1 arg2))
     (make-Integer-Literal (expt (Integer-Literal-val arg1)
                                 (Integer-Literal-val arg2)))]))

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
;(provide eval-expression
;         eval-predicate)
;
;(define-namespace-anchor a)
;(define this-namespace (namespace-anchor->namespace a))
;
;(define (integer-symbol? s)
;  (eq? s 'integer))
;(define (natural-symbol? s)
;  (eq? s 'natural))
;(define (natural1-symbol? s)
;  (eq? s 'natural1))
;(define (var-symbol? s)
;  (and (symbol? s) (string-prefix? "var:" (symbol->string s))))
;(define (const-symbol? s)
;  (and (symbol? s) (string-prefix? "const:" (symbol->string s))))
;(define (set-literal-symbol? s)
;  (and (symbol? s) (string-prefix? "slit:" (symbol->string s))))
;(define (boolean-symbol? s)
;  (and (symbol? s) (or (eq? s 'true) (eq? s 'false))))
;(define (set? s)
;  (and (symbol? s) (string-prefix? "set:" (symbol->string s))))
;(define (emptyset-symbol? s)
;  (and (symbol? s) (eq? s 'emptyset)))
;
;(define eval-expression
;  (case-lambda 
;    [(sexpr state1 state2 . states)
;     (eval-expression sexpr (apply dict-merge state1 state2 states))]
;    [(sexpr state)
;     (fprintf (current-error-port) "eval-expression: ~a~nstate: ~a~n~n" sexpr state)
;     (cond [(list? sexpr)
;            (apply (eval (first sexpr) this-namespace)
;                   (map (lambda (se) (eval-expression se state)) (rest sexpr)))]
;           [(integer-symbol? sexpr) (make-integer-set)]
;           [(natural-symbol? sexpr) (make-natural-set)]
;           [(natural1-symbol? sexpr) (make-natural1-set)]
;           [(integer? sexpr) sexpr]
;           [(or (var-symbol? sexpr) (const-symbol? sexpr)) (dict-ref state sexpr)]
;           [(or (set-literal-symbol? sexpr) (boolean-symbol? sexpr)) sexpr]
;           ;; Sets evaluate to their set enumeration definition
;           [(set? sexpr) (list->eb-set (dict-ref state sexpr))]
;           [(emptyset-symbol? sexpr) (make-empty-set)]
;           [else (error 'eval-expression "Unexpected expression to evaluate: ~a" sexpr)])]))
;
;;; Implements lazy evaluation of predicates for 'and and 'or
;;; - 'and only evaluates its arguments till it finds a false
;;; - 'or only evaluates its arguments till it finds a true
;;; 
;(define eval-predicate 
;  (case-lambda
;    [(sexpr state1 state2 . states)
;     (eval-predicate sexpr (apply dict-merge state1 state2 states))]
;    [(sexpr state)
;     (fprintf (current-error-port) "eval-predicate: ~a~nstate: ~a~n~n" sexpr state)
;     (cond [(and (list? sexpr) (eq? (first sexpr) 'eb-land))
;            (and (eval-predicate (second sexpr) state)
;                 (eval-predicate (third sexpr) state))]
;           [(and (list? sexpr) (eq? (first sexpr) 'eb-lor))
;            (or (eval-predicate (second sexpr) state)
;                (eval-predicate (third sexpr) state))]
;           [(list? sexpr)
;            (apply (eval (first sexpr) this-namespace)
;                   (if (memq (first sexpr) '(eb-in eb-notin eb-subset eb-subseteq eb-notsubset eb-notsubseteq eb-equal eb-notequal eb-lt eb-gt eb-leq eb-geq))  
;                       (map (lambda (se) (eval-expression se state)) (rest sexpr))
;                       (map (lambda (se) (eval-predicate se state)) (rest sexpr))))]
;           [(boolean? sexpr) sexpr]
;           [else (eval-expression sexpr state)])]))
;
;
;;                                                                                
;;                                                     ;                          
;;    ;;;;  ;           ;                ;;;;;                    ;               
;;   ;;   ; ;           ;                ;   ;;                   ;               
;;   ;    ;;;;;  ;;;; ;;;;;   ;;;        ;    ; ;;;; ;;;   ;;;; ;;;;;   ;;;   ;;;;
;;    ;;;   ;        ;  ;    ;   ;       ;   ;; ;; ;   ;   ;   ;  ;    ;   ;  ;; ;
;;      ;;; ;     ;;;;  ;    ;;;;;       ;;;;;  ;      ;   ;   ;  ;    ;;;;;  ;   
;;        ; ;    ;   ;  ;    ;           ;      ;      ;   ;   ;  ;    ;      ;   
;;   ;    ; ;    ;  ;;  ;    ;;          ;      ;      ;   ;   ;  ;    ;;     ;   
;;    ;;;;  ;;;   ;;;;  ;;;   ;;;;       ;      ;    ;;;;; ;   ;  ;;;   ;;;;  ;   
;;                                                                                
;;                                                                                
;;                                                                                
;
;
;(define (print-state state (out (current-output-port)))
;  (letrec ([print-value
;            (lambda (val (out (current-output-port)))
;              (cond [(or (integer? val) (symbol? val)) (fprintf out "~a" val)]
;                    [(pair? val) 
;                     (print-value (car val) out)
;                     (fprintf out " |-> ")
;                     (print-value (cdr val) out)]
;                    [(set:set? val)
;                     (fprintf out "{~a}"
;                              (apply string-append 
;                                     (add-between (map (lambda (v)
;                                                         (let ([o (open-output-string)])
;                                                           (print-value v o)
;                                                           (get-output-string o)))
;                                                       (set:elements val))
;                                                  ", ")))]
;                    [else
;                     (error 'print-state "Can't print value of state: " val)]))])
;    (parameterize ([current-output-port out])
;      (for-each (match-lambda ((cons var val) 
;                               (printf "~a : " var)
;                               (print-value val out)
;                               (newline out)))
;                state))))