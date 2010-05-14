#lang scheme

(require (only-in srfi/1 every)
         "ast.scm"
         "types.scm"
         "typing-rules.scm"
         "environment.scm")

;; The environment passed around associates a variable to a type
;; that corresponds to it. It is basically an (assoc (symbol? . Type?) ...).
(define (pass-machine ast (env (make-empty-env symbol? symbol=?)))
  (match ast
    ((struct Machine (name sees vars events invs variants theorems initialisation))
     (let* ([context/env-list (map (lambda (c) (pass-context c env)) sees)]
            [typed-contexts (map car context/env-list)]
            [initial-env (if (null? context/env-list)
                             env
                             (apply env-merge (map cdr context/env-list)))])
       (let-values ([(typed-invs typed-vars) 
                     (type-and-extract-typed-ids invs vars initial-env)])
         (let* ([machine-env ;; Adds contextual information to environment and state variables
                 (foldl (lambda (typed-var acum-env)
                          (env-add-binding acum-env 
                                           (Variable-name (Expr/wt-expr typed-var))
                                           (Expr/wt-type typed-var)))
                        initial-env
                        (append typed-vars (map typed-variable->typed-post-variable typed-vars)))])
           (let ([typed-events (map (lambda (ev) (pass-event ev machine-env)) events)]
                 [typed-variants (map (lambda (variant) (pass-expression/retype variant machine-env))
                                      variants)]
                 [typed-theorems (map (lambda (thm) 
                                        (make-Labelled (Labelled-label thm)
                                                       (pass-predicate/retype (Labelled-struct thm) machine-env)))
                                      theorems)]
                 [typed-inits (map (lambda (act) 
                                     (make-Labelled (Labelled-label act)
                                                    (pass-action (Labelled-struct act) machine-env)))  ;; Type Initialisation
                                   initialisation)])
             (make-Machine name typed-contexts typed-vars typed-events typed-invs typed-variants typed-theorems typed-inits))))))))


(define/contract (type-and-extract-typed-ids labelled-preds ids env)
  ((listof labelled-predicate?) (listof Identifier?) Environment? . -> . (values (listof labelled-typed-predicate?) (listof typed-identifier?)))
  (let* ([preds (map Labelled-struct labelled-preds)]
         [typed-preds (pass-listof-predicate/retype preds env)])
    (values (map make-Labelled (map Labelled-label labelled-preds) typed-preds)
            (infer-identifier-types ids typed-preds))))

;; The passing of a context is very important. 
;; Constants are identifier whose type are inferred from the axioms. So we need to type the axioms
;; and as soon as we determine a type for each of the constants we attach the type to the identifer
;; of the constant.
;;
(define/contract (infer-identifier-types ids tpe)
  ((listof Identifier?) (listof (or/c typed-predicate? typed-expression?)) . -> . (listof typed-identifier?))
  (let loop ([typed-ids '()] [rest-ids ids] [rest-tpe tpe])
    (cond [(null? rest-ids) typed-ids]
          [(null? rest-tpe)
           (error 'infer-identifier-types
                  "Couldn't infer types for all requested identifiers. Managed to find types for: ~a, but ~a still missing." typed-ids rest-ids)]
          [(typed-identifier? (first rest-tpe))
           (let ([ti (first rest-tpe)])
             (if (findf (lambda (el) (identifier=? el (Expr/wt-expr ti))) rest-ids)
                 (loop (cons ti typed-ids)
                       (remove (Expr/wt-expr ti) rest-ids identifier=?)
                       (rest rest-tpe))
                 (loop typed-ids rest-ids (rest rest-tpe))))]
          [else
           (let* ([element (first rest-tpe)])
             (match (if (predicate? element) element (Expr/wt-expr element))
               ;; Expression matching
               ((or (struct Expression-Literal _) (struct Integer-Literal _)) 
                (loop typed-ids rest-ids (rest rest-tpe)))
               ((struct Expression-Bool (pred)) 
                (loop typed-ids rest-ids (cons pred (rest rest-tpe))))
               ((struct Expression-UnOp (_ arg))
                (loop typed-ids rest-ids (cons arg (rest rest-tpe))))
               ((struct Expression-BinOp (_ arg1 arg2)) 
                (loop typed-ids rest-ids (list* arg1 arg2 (rest rest-tpe))))
               ((struct Lambda-Expression (id-pat pred expr))
                (loop typed-ids rest-ids (list* id-pat pred expr (rest rest-tpe))))
               ((struct Set-Comprehension (_ _ pred expr))
                (loop typed-ids rest-ids (list*  pred expr (rest rest-tpe))))
               ((struct Set-Enumeration (exprs))
                (loop typed-ids rest-ids (append exprs (rest rest-tpe))))
               ((struct Variable-Pair (car cdr))
                (loop typed-ids rest-ids (list* car cdr (rest rest-tpe))))
               
               ;; Predicate matching
               ((struct Predicate-Literal _) 
                (loop typed-ids rest-ids (rest rest-tpe)))
               ((struct Predicate-UnOp (_ arg)) 
                (loop typed-ids rest-ids (cons arg (rest rest-tpe))))
               ((struct Predicate-BinOp (_ arg1 arg2)) 
                (loop typed-ids rest-ids (list* arg1 arg2 (rest rest-tpe))))
               ((struct Predicate-RelOp (_ arg1 arg2))
                (loop typed-ids rest-ids (list* arg1 arg2 (rest rest-tpe))))
               ((struct Quantifier (_ _ body))
                (loop typed-ids rest-ids (cons body (rest rest-tpe))))))])))

(define (pass-context ast env) 
  (match ast
    ((struct Context (name extensions sets constants axioms theorems))
     (let* ([typed-context/env-pair (map (lambda (c) (pass-context c env)) extensions)]
            [typed-contexts (map car typed-context/env-pair)]
            [envs (map cdr typed-context/env-pair)]
            [merged-envs (apply env-merge env envs)]
            [axioms-env ;; The env to type needs to have knowledge about the enumerated sets,
             ;; so I add them to the final environment generated by the contexts.
             (foldl (lambda (es env-acum)
                      ;; for each enumerated set we add all its elements to the environment
                      ;; and we add the for each set name, its type to the environment.
                      ;; So, for each set s := {s1, ...}
                      ;; we add: s -> POW(s:(s1,...)) 
                      ;; and s1 -> s:(s1, ...), s2 -> s:(s1, ...) etc ...
                      (env-add-binding 
                       (foldl (lambda (sym v-env-acum) 
                                (env-add-binding v-env-acum sym 
                                                 (make-Type-Enumeration (Set-name es) 
                                                                        (map Set-Literal-name (Enumerated-Set-vals es)))))
                              env-acum
                              (map Set-Literal-name (Enumerated-Set-vals es)))
                       (Set-name es)
                       (make-Type-Powerset (make-Type-Enumeration (Set-name es) (map Set-Literal-name (Enumerated-Set-vals es))))))
                    merged-envs
                    sets)]) ;; There should only be enumerated sets, so we assume here there are no deferred sets
                            ;; otherwise these line would be (filter Enumerated-Set? set) and treat the 
                            ;; deferred sets in some different way.
       (let-values ([(typed-axioms typed-constants) (type-and-extract-typed-ids axioms constants axioms-env)])
         (let* ([final-env ;; this is the env used for theorems and returned by the context with information about everything in it
                 (foldl (lambda (tc env-acum)
                          (env-add-binding env-acum (Constant-name (Expr/wt-expr tc))
                                           (Expr/wt-type tc)))
                        axioms-env
                        typed-constants)])
           (cons 
            (make-Context name
                          typed-contexts
                          sets
                          typed-constants
                          typed-axioms
                          (map (lambda (lpred) 
                                 (make-Labelled (Labelled-label lpred)
                                                (pass-predicate/retype (Labelled-struct lpred) final-env)))
                               theorems))
            final-env)))))))

;; When typing events we need to make sure we pass enough information to the rhs for retyping.
;; For example in the following action, this won't type correctly:
;; x := {}
;; Simply because {} on its own doesn't type. We need to tell it explicitly, HEY, 
;; we know that the type of {} must match the type of x and the type of x is well-known
;; because it needs to be a state variable.
(define (pass-event ast env)
  (match ast
    ((struct Event (name status locals guards actions))
     ;; An event creates a new environment
     (let-values ([(typed-guards typed-locals)
                   (type-and-extract-typed-ids guards locals env)])
       (let ([event-env (foldl (lambda (var env-accum) 
                                 (env-add-binding env-accum 
                                                  (Variable-name (Expr/wt-expr var))
                                                  (Expr/wt-type var)))
                               env
                               typed-locals)])
         (make-Event name
                     status
                     typed-locals
                     typed-guards
                     (map (lambda (act) 
                            (make-Labelled (Labelled-label act)
                                           (match (Labelled-struct act)
                                             ((struct Assign-Action (lhs rhs))
                                              ;; TODO , ASSIGN THE TYPES OF THE LHS TO THE RHS
                                              (make-Assign-Action (map (lambda (v) (pass-expression/retype v env)) lhs) 
                                                                  (map (lambda (e) (pass-expression/retype e event-env)) rhs)))
                                             ((struct In-Assign-Action (lhs rhs))
                                              ;; TODO , ASSIGN THE TYPE OF THE LHS TO THE RHS
                                              (make-In-Assign-Action (pass-expression/retype lhs env)
                                                                     (pass-expression/retype rhs event-env)))
                                             ((struct Suchthat-Assign-Action (lhs rhs))
                                              (make-Suchthat-Assign-Action (map (lambda (v) (pass-expression/retype v env)) lhs) 
                                                                           (pass-predicate/retype rhs event-env))))))
                          actions)))))))

;(: pass-action (Action Environment -> Typed-Action))
;; TODO Why not use pass-action in pass-event????????????????????????????????
(define (pass-action act env)
  (match act
    ((struct Assign-Action (vars exprs))
     (let ([typed-vars (map (lambda (v) (pass-expression/retype v env)) vars)])
       (make-Assign-Action 
        typed-vars
        (map (lambda (expr type) 
               ;; Here we cannot use pass-expression/retype
               ;; because after the initial typing pass we need to add typing information
               ;; so that the retype finishes successfully.
               (let-values ([(expr-vars expr-env expr-eqs typed-expr) (pass-expression expr '() env '())])
                 (retype-expression typed-expr 
                                    expr-vars 
                                    expr-env
                                    (cons (cons (Expr/wt-type typed-expr) type) expr-eqs))))
             exprs 
             (map Expr/wt-type typed-vars)))))
    ((struct In-Assign-Action (var expr))       
     (let ([typed-var (pass-expression/retype var env)])
       (make-In-Assign-Action 
        typed-var
        ;; Again, we cannot use pass-expression/retype for the same reasons as above.
        (let-values ([(expr-vars expr-env expr-eqs typed-expr) (pass-expression expr '() env '())])
          (retype-expression typed-expr
                             expr-vars
                             expr-env
                             (cons (cons (Expr/wt-type typed-expr) 
                                         (Expr/wt-type typed-var)) expr-eqs))))))
    ((struct Suchthat-Assign-Action (var-list pred))
     (make-Suchthat-Assign-Action 
      (map (lambda (var) (pass-expression/retype var env)) var-list)
      (pass-predicate/retype pred env)))))

(define (pass-predicate/retype pred env)
  (let-values ([(pred-vars pred-env pred-eqs typed-pred) (pass-predicate pred '() env '())])
    (retype-predicate typed-pred pred-vars pred-env pred-eqs)))

;; Types and retypes a list of predicates
(define (pass-listof-predicate/retype ps env)
  (let loop ([rest-ps ps]
             [accum-vars '()]
             [accum-env env]
             [accum-eqs '()]
             [typed-ps '()])
    (if (null? rest-ps)
        (map (lambda (tp) (retype-predicate tp accum-vars accum-env accum-eqs)) typed-ps)
        (let-values ([(p-vars p-env p-eqs typed-p) (pass-predicate (first rest-ps) accum-vars accum-env accum-eqs)])
          (loop (rest rest-ps) 
                p-vars
                p-env
                p-eqs
                (cons typed-p typed-ps))))))

; The following function passes a predicate and infers the type of each predicate by following 
; the algorithm described in the mathematical language document for event-b.
; vars is a list of Type-Polymorphic
; env is an environment which associates a variable with a type
; eqs is a list of pairs of types which represent a system of type equations
;
; This function returns 4 values, a set of synthesized variables,
; a synthesized environment, a list of synthesized equations and a new predicate whose sub-expressions are typed.
(define (pass-predicate ast vars env eqs)
  (match ast
    ;    pred-lit: P ::= [pred-lit]
    ;      P.synthesized = P.inherited
    ((struct Predicate-Literal _)
     (values vars env eqs ast))
    
    ;pred-quant: P ::= L1 P1 [pred-quant]
    ;  L1 .inherited = P.inherited
    ;  P1 .inherited = L1 .synthesized
    ;  P.synthesized = P1 .synthesized
    ((struct Quantifier (quant var body))
     (let*-values ([(var-vars var-env var-eqs typed-var) (pass-expression var vars env eqs)]
                   [(body-vars body-env body-eqs typed-body) (pass-predicate body var-vars var-env var-eqs)])
       (values body-vars body-env body-eqs
               (make-Quantifier quant typed-var typed-body))))
    
    
    ;   pred-una: P ::= P1
    ;      P1 .inherited = P.inherited
    ;      P.synthesized = P1 .synthesized
    ((struct Predicate-UnOp (op arg))
     (let-values ([(arg-vars arg-env arg-eqs typed-arg) (pass-predicate arg vars env eqs)])
       (values arg-vars arg-env arg-eqs 
               (p `(,op ,typed-arg)))))
    
    ;    pred-bin: P ::= P1 P2 [pred-binop]
    ;      P1 .inherited = P.inherited
    ;      P2 .inherited = P1 .synthesized
    ;      P.synthesized = P2 .synthesized
    ((struct Predicate-BinOp (op arg1 arg2))
     (let*-values ([(arg1-vars arg1-env arg1-eqs typed-arg1) (pass-predicate arg1 vars env eqs)]
                   [(arg2-vars arg2-env arg2-eqs typed-arg2) (pass-predicate arg2 arg1-vars arg1-env arg1-eqs)])
       (values arg2-vars arg2-env arg2-eqs 
               (p `(,typed-arg1 ,op ,typed-arg2))))) 
    ;    pred-rel: P ::= E1 E2 [pred-relop]
    ; Let α be a fresh type variable in
    ;       E1 .ityvars = P.ityvars ∪ {α}
    ;       E1 .ityenv = P.ityenv
    ;       E1 .ityeqs = P.ityeqs
    ;       E2 .inherited = E1 .synthesized
    ;       P.styvars = E2 .styvars
    ;       P.styenv = E2 .styenv
    ;       P.styeqs = E2 .styeqs ∪ E
    ; where E is defined in the following table.
    ((struct Predicate-RelOp (op arg1 arg2))
     (let ([fresh-var (make-fresh-type-variable)])
       (let*-values ([(arg1-vars arg1-env arg1-eqs typed-arg1) (pass-expression arg1 (cons fresh-var vars) env eqs)]
                     [(arg2-vars arg2-env arg2-eqs typed-arg2) (pass-expression arg2 arg1-vars arg1-env arg1-eqs)])
         (values arg2-vars arg2-env 
                 (append arg2-eqs ;; the equations to append depend on the op
                         (case op
                           [(equal notequal) (list (cons (Expr/wt-type typed-arg1) fresh-var)
                                                   (cons (Expr/wt-type typed-arg2) fresh-var))]
                           [(lt gt ge le) (list (cons (Expr/wt-type typed-arg1) (make-Type-Integer))
                                                (cons (Expr/wt-type typed-arg2) (make-Type-Integer)))]
                           [(in notin) (list (cons (Expr/wt-type typed-arg1) fresh-var)
                                             (cons (Expr/wt-type typed-arg2) (make-Type-Powerset fresh-var)))]
                           [(subset subseteq notsubset notsubseteq) (list (cons (Expr/wt-type typed-arg1) (make-Type-Powerset fresh-var))
                                                                          (cons (Expr/wt-type typed-arg2) (make-Type-Powerset fresh-var)))]))
                 (p `(,typed-arg1 ,op ,typed-arg2))))))))

(define (pass-expression/retype expr env)
  (let-values ([(expr-vars expr-env expr-eqs typed-expr) (pass-expression expr '() env '())])
    (retype-expression typed-expr expr-vars expr-env expr-eqs)))

; The following function passes an expression and infers the type of each expression by following 
; the algorithm described in the mathematical language document for event-b.
; vars is a list of Type-Polymorphic
; env is an environment which associates a variable with a type
; eqs is a list of pairs of types which represent a system of type equations
;
; This function returns 4 values, a set of synthesized variables,
; a synthesized environment, a list of synthesized equations and a Expr/wt
; with the expression structure and a polymorphic type associated with it.
(define (pass-expression ast vars env eqs) 
  (match ast
    ;    expr-int: E ::= [int-lit]
    ;     E.synthesized = E.inherited
    ;            E.type = Z
    ((struct Integer-Literal (val))
     (values vars env eqs
             (make-Expr/wt (make-Type-Integer) ast)))
    
    ;    expr-atom: E ::= [expr-lit]
    ; Let α and β be distinct fresh type variables in
    ;     E.styvars = E.ityvars ∪ {α, β}
    ;     E.styenv = E.ityenv
    ;     E.styeqs = E.ityeqs
    ;     E.type = τ [τ is defined in a table in the event-b document]
    ((struct Expression-Literal (lit))
     (let-values ([(new-vars new-eqs expr-type) (eventb-expr-lit-type-rules lit)])
       (values (append new-vars vars) 
               env 
               (append new-eqs eqs)
               (make-Expr/wt expr-type ast))))
    
    ((or (struct Variable _)
         (struct Constant _)
         (struct Set-Literal _)
         (struct Set _))
     (pass-ident ast vars env eqs))
    
    ;     expr-una: E ::= E1 [expr-unop]
    ; Let α and β be distinct fresh type variables in
    ;     E1 .ityvars = E.ityvars ∪ {α, β}
    ;     E1 .ityenv = E.ityenv
    ;     E1 .ityeqs = E.ityeqs
    ;     E.styvars = E1 .styvars
    ;     E.styenv = E1 .styenv
    ;     E.styeqs = E1 .styeqs ∪ E
    ;     E.type = τ
    ((struct Expression-UnOp (op arg))
     (let*-values ([(arg-vars arg-env arg-eqs typed-arg) (pass-expression arg vars env eqs)]
                   [(new-vars new-eqs expr-type) (eventb-unary-expr-type-rules op (Expr/wt-type typed-arg))]) ;; Still wondering if not calling pass-expr on arg with new-vars is a problem... ?!?
       (values (append arg-vars new-vars) 
               arg-env 
               (append new-eqs arg-eqs)
               (make-Expr/wt expr-type (e `(,op ,typed-arg))))))
    
    ;     expr-bin: E ::= E1 E2 [expr-binop]
    ; Let α, β, γ and δ be distinct fresh type variables in
    ;       E1 .ityvars = E.ityvars ∪ {α, β, γ, δ}
    ;       E1 .ityenv = E.ityenv
    ;       E1 .ityeqs = E.ityeqs
    ;       E2 .inherited = E1 .synthesized
    ;       E.styvars = E2 .styvars
    ;       E.styenv = E2 .styenv
    ;       E.styeqs = E2 .styeqs ∪ E
    ;       E.type = τ
    ((struct Expression-BinOp (op arg1 arg2))
     (let*-values ([(arg1-vars arg1-env arg1-eqs typed-arg1) (pass-expression arg1 vars env eqs)]
                   [(arg2-vars arg2-env arg2-eqs typed-arg2) (pass-expression arg2 arg1-vars arg1-env arg1-eqs)]
                   [(new-vars new-eqs expr-type) (eventb-bin-expr-type-rules op (Expr/wt-type typed-arg1) (Expr/wt-type typed-arg2))])
       (values (append new-vars arg2-vars) 
               arg2-env 
               (append new-eqs arg2-eqs) 
               (make-Expr/wt expr-type (e `(,typed-arg1 ,op ,typed-arg2))))))
    
    ;    expr-lambda: E ::= Q1 P1 E1
    ;       Q1 .inherited = E.inherited
    ;       P1 .inherited = Q1 .synthesized
    ;       E1 .inherited = P1 .synthesized
    ;       E.synthesized = E1 .synthesized
    ;       E.type = P(Q1.type × E1.type)
    ;
    ((struct Lambda-Expression (id-pattern pred expr))
     (let*-values ([(pat-vars pat-env pat-eqs typed-pat) (pass-expression id-pattern vars env eqs)]
                   [(pred-vars pred-env pred-eqs typed-pred) (pass-predicate pred pat-vars pat-env pat-eqs)]
                   [(expr-vars expr-env expr-eqs typed-expr) (pass-expression expr pred-vars pred-env pred-eqs)])
       (values expr-vars expr-env expr-eqs
               (make-Expr/wt (t `(P (,(Expr/wt-type typed-pat) x ,(Expr/wt-type typed-expr))))
                             (make-Lambda-Expression typed-pat typed-pred typed-expr))))) 
    
    ((struct Variable-Pair (pair-car pair-cdr))
     (let*-values ([(car-vars car-env car-eqs typed-car) (pass-expression pair-car vars env eqs)]
                   [(cdr-vars cdr-env cdr-eqs typed-cdr) (pass-expression pair-cdr car-vars car-env car-eqs)])
       (make-Expr/wt (t `(,(Expr/wt-type typed-car) x ,(Expr/wt-type typed-cdr)))
                     (make-Variable-Pair typed-car typed-cdr))))
    
    ;    expr-quant1: E ::= L1 P1 E1 [expr-quant]
    ;    Let α be a fresh type variable in
    ;       L1 .ityvars = E.ityvars ∪ {α}
    ;       L1 .ityenv = E.ityenv
    ;       L1 .ityeqs = E.ityeqs
    ;       P1 .inherited = L1 .synthesized
    ;       E1 .inherited = P1 .synthesized
    ;       E.styvars = E1 .styvars
    ;       E.styenv = E1 .styenv
    ;       E.styeqs = E1 .styeqs ∪ E
    ;       E.type = τ
    ;    where E and τ are defined in the following table.
    ;
    ((struct Set-Comprehension (comp-op vs pred expr))
     (let ([fresh-var (make-fresh-type-variable)])
       (let*-values ([(vs-vars vs-env vs-eqs typed-vs) (pass-listof-ident vs (cons fresh-var vars) env eqs)]
                     [(pred-vars pred-env pred-eqs typed-pred) (pass-predicate pred vs-vars vs-env vs-eqs)]
                     [(expr-vars expr-env expr-eqs typed-expr) (pass-expression expr pred-vars pred-env pred-eqs)]
                     [(comp-eqs comp-type) (eventb-expr-quant-type-rules comp-op (Expr/wt-type typed-expr))])
         (values expr-vars expr-env (append eqs comp-eqs) 
                 (make-Expr/wt comp-type 
                               (make-Set-Comprehension comp-op typed-vs typed-pred typed-expr))))))
    
    
    ;    expr-eset: E ::= M1
    ;       M1 .inherited = E.inherited
    ;       E.synthesized = M1 .synthesized
    ;       E.type = P(M1 .type)
    ;
    ((struct Set-Enumeration (exprs))
     (letrec ([gen-eq-pairs
               (lambda (ts) ;; List of types, generates the equatins required for an Expression List
                 (let loop ([rest-ts ts] [eqs '()])
                   (if (or (null? rest-ts) (null? (rest rest-ts)))
                       eqs
                       (loop (rest rest-ts) 
                             (cons (cons (first rest-ts) (first (rest rest-ts))) eqs)))))])
       (let loop ([rest-expr exprs] ;; Expression to loop through
                  [vars-acum vars]
                  [env-acum env]
                  [eqs-acum eqs]
                  [typed-exprs '()])
         (if (null? rest-expr)
             (values 
              vars-acum
              env-acum
              eqs-acum
              (make-Expr/wt (t `(P ,(Expr/wt-type (first typed-exprs))))
                            (make-Set-Enumeration (reverse typed-exprs))))
             (let-values ([(expr-vars expr-env expr-eqs typed-expr) (pass-expression (first rest-expr) vars-acum env-acum eqs-acum)])
               (loop (rest rest-expr)
                     (append expr-vars vars-acum)
                     (env-merge/check type=? expr-env env-acum)
                     (append expr-eqs eqs-acum)
                     (cons typed-expr typed-exprs)))))))))


; ident-list: L ::= I1 I2 . . . In
;     I1 .inherited = L.inherited
;     I2 .inherited = I1 .synthesized
;                   .
;                   .
;                   .
;     In .inherited = In−1 .synthesized
;     L.synthesized = In .synthesized
;
(define (pass-listof-ident vs vars env eqs)
  (let loop ([rest-vs vs]
             [accum-vars vars]
             [accum-env env]
             [accum-eqs eqs]
             [typed-vs '()])
    (if (null? rest-vs)
        (values accum-vars accum-env accum-eqs typed-vs)
        (let-values ([(v-vars v-env v-eqs typed-v) (pass-ident (first rest-vs) accum-vars accum-env accum-eqs)])
          (loop (rest rest-vs) 
                v-vars
                v-env
                v-eqs
                (cons typed-v typed-vs))))))

;ident: I ::= [name]
;  if I.name ∈ dom(I.ityenv) then
;      I.synthesized = I.inherited
;      I.type = I.ityenv(I.name)
;  else let α be a fresh type variable in
;      I.styvars = I.ityvars ∪ {α}
;      I.styenv = I.ityenv ∪ {I.name → α}
;      I.styeqs = I.ityeqs
;      I.type = α
;
(define (pass-ident v vars env eqs)
  (let ([binding (env-get-binding env (Identifier-name v))])
    (if binding
        (values vars env eqs (make-Expr/wt (cdr binding) v))
        (let ([new-type-var (make-fresh-type-variable)])
          (values (cons new-type-var vars)
                  (env-add-binding env (Identifier-name v) new-type-var)
                  eqs
                  (make-Expr/wt new-type-var v))))))

;; retype-predicate is a function that receives a predicate and retypes it,
;; so that the resulting predicate contains only typed subexpressions without 
;; type variables. It receives the same values returned by pass-predicate:
;; a predicate to retype [however, it should be fully typed, containing, possibly, type variables]
;; a list of type variables
;; an environment associating variables in the predicate to typed-variables
;; a set of type equations that have to be solved to assign a type to each type variable.
;; Once each type variable has a type, the predicate is recursively changed so that 
;; each variable/sub-expression has its own ground type.
(define (retype-predicate pred vars env eqs)
  (let ([subst (unify-types/uniquely eqs)])
    (unless subst
      (error 'retype-predicate 
             "Found type error in predicate. Cannot solve type equations.~npred: ~a~nvars: ~a~nenv: ~a~neqs: ~a~n" pred vars env eqs))
    (remove-type-vars/pred pred subst)))

(define (retype-expression expr vars env eqs)
  (let ([subst (unify-types/uniquely eqs)])
    (unless subst
      (error 'retype-expression
             "Found type error in expression. Cannot solve type equations.~nexpr: ~a~nvars: ~a~nenv: ~a~neqs: ~a~n" expr vars env eqs))
    (remove-type-vars/expr expr subst)))

(define (remove-type-vars/pred pred subst)
  (match pred
    ((struct Predicate-Literal _) pred)
    ((struct Predicate-UnOp (op arg)) 
     (make-Predicate-UnOp op (remove-type-vars/pred arg subst)))
    ((struct Predicate-BinOp (op arg1 arg2)) 
     (make-Predicate-BinOp op 
                           (remove-type-vars/pred arg1 subst) 
                           (remove-type-vars/pred arg2 subst)))
    ((struct Predicate-RelOp (op arg1 arg2)) 
     (make-Predicate-RelOp op 
                           (remove-type-vars/expr arg1 subst) 
                           (remove-type-vars/expr arg2 subst)))
    ((struct Quantifier (quant var p))
     (make-Quantifier quant
                      (remove-type-vars/expr var subst)
                      (remove-type-vars/pred p subst)))))

(define (remove-type-vars/expr e subst)
  (let* ([type (Expr/wt-type e)]
         [expr (Expr/wt-expr e)]
         [ground-type (type->ground-type type subst)])
    (make-Expr/wt 
     ground-type
     (match expr
       ((or (struct Integer-Literal _)
            (struct Expression-Literal _)
            (struct Variable _)
            (struct Constant _)
            (struct Set-Literal _)
            (struct Set _))
        expr)
       ((struct Expression-Bool (pred)) 
        (remove-type-vars/pred pred subst))
       ((struct Expression-UnOp (op arg))
        (make-Expression-UnOp op (remove-type-vars/expr arg subst)))
       ((struct Expression-BinOp (op arg1 arg2))
        (make-Expression-BinOp op 
                               (remove-type-vars/expr arg1 subst)
                               (remove-type-vars/expr arg2 subst)))
       ((struct Lambda-Expression (pat pred expr))
        (make-Lambda-Expression (remove-type-vars/expr pat subst)
                                (remove-type-vars/expr pred subst)
                                (remove-type-vars/expr expr subst)))
       ((struct Set-Comprehension (op vars pred expr))
        (make-Set-Comprehension op
                                (map (lambda (v) (remove-type-vars/expr v subst)) vars)
                                (remove-type-vars/pred pred subst)
                                (remove-type-vars/expr expr subst)))
       ((struct Set-Enumeration (exprs))
        (make-Set-Enumeration (map (lambda (e) (remove-type-vars/expr e subst)) exprs)))
       ((struct Variable-Pair (car cdr))
        (make-Variable-Pair (remove-type-vars/expr car))
        (make-Variable-Pair (remove-type-vars/expr cdr)))))))


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
 [rename pass-machine machine/type (Machine? . -> . typed-machine?)])
