#lang racket
;;
;;    This file is part of Eboc (Event-B Model Checker).
;;
;;    Eboc is free software: you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation, either version 3 of the License, or
;;    (at your option) any later version.
;;
;;    Eboc is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.
;;
;;    You should have received a copy of the GNU General Public License
;;    along with Eboc.  If not, see <http://www.gnu.org/licenses/>.
;;

(require srfi/1
         parser-tools/lex
         parser-tools/yacc
         "types.rkt"
         "ast.rkt"
         "params.rkt"
         "macro-utils.rkt")

(require (prefix-in : parser-tools/lex-sre))

(provide parse-machine)

;; Name of current file being parsed
(define current-file (make-parameter "<parser:current-file:ERROR>"))

;                                     
;                                     
;                                     
;   ;                                 
;   ;                                 
;   ;       ;;;   ;   ;   ;;;    ; ;;;
;   ;      ;;  ;   ; ;   ;;  ;   ;;   
;   ;      ;   ;   ;;;   ;   ;   ;    
;   ;      ;;;;;    ;    ;;;;;   ;    
;   ;      ;       ;;;   ;       ;    
;   ;      ;      ;; ;;  ;       ;    
;   ;;;;;;  ;;;;  ;   ;   ;;;;   ;    
;                                     
;                                     
;                                     


(define-empty-tokens pred-tokens
  (LEFTPAREN
   RIGHTPAREN
   EQUIV
   IMPL
   AND
   OR
   NOT
   BOTTOM
   TOP
   FORALL
   EXISTS
   QDOT))

(define-empty-tokens expr->pred-tokens
  (PARTITION
   FINITE
   EQ
   NOTEQ
   LESS
   LEQ
   GREATER
   GEQ
   IN
   NOTIN
   SUBSET
   NOTSUBSET
   SUBSETEQ
   NOTSUBSETEQ))

(define-empty-tokens sets-relations-tokens
  (REL
   TREL
   SREL
   STREL
   PFUN
   TFUN
   PINJ
   TINJ
   PSUR
   TSUR
   BIJ))

(define-empty-tokens set-manip-tokens
  (LEFTCURLY
   RIGHTCURLY
   MAPLET
   EMPTYSET
   INTERSECTION
   UNION
   SETMINUS
   CARTPROD
   BIGUNION
   BIGINTER))

(define-empty-tokens rel-manip-tokens
  (LEFTSQ
   RIGHTSQ
   RELOVL
   BCOMP
   FCOMP
   DIRPROD
   PARPROD
   TILDEOP
   DOMSUB
   DOMRES
   RANSUB
   RANRES))

(define-empty-tokens quant-expr-tokens
  (LAMBDA
   NARY-INTER
   NARY-UNION
   SUCHTHAT))

(define-empty-tokens arith-expr-tokens
  (UPTO
   PLUS
   MINUS
   UMINUS
   MULT
   DIV
   EXP))

(define-empty-tokens str-tokens
  (BOOL
   BOOLFN
   PRJ1
   PRJ2
   FALSE
   CARD
   MAX
   TRUE
   DOM
   MIN
   RAN
   MOD
   SUCC
   ID
   PRED))

(define-empty-tokens non-ascii-ids-tokens
  (NAT1
   NAT
   INT
   POWERSET
   POWERSET1
   OFTYPE))

(define-tokens ids-numbers-tokens
  (NUM
   IDENTIFIER
   LABEL))

(define-empty-tokens language-tokens
  (COMMA
   END
   MEND
   CONTEXT
   MACHINE
   INITIALISATION
   EVENT
   AXIOMS
   THEOREMS
   CONSTANTS
   SETS
   SEES
   VARIABLES
   ASSIGN
   IN-ASSIGN
   SUCHTHAT-ASSIGN
   CONVERGENT
   NORMAL
   ANY
   WHERE
   WHEN
   THEN
   INVARIANTS
   VARIANTS
   EXTENDS
   STATUS
   DEFI))

(define-empty-tokens special-tokens
  (EOF NEWLINE))

(define-lex-abbrevs
  (line-comment (:: "//" (complement (:: any-string "\n" any-string)) "\n"))
  (c-style-comment (:: "/*" (complement (:: any-string "*/" any-string)) "*/"))
  (digit (:/ "0" "9"))
  (int-literal (:+ digit))
  (label (:: "@" (:: alphabetic (:* (union alphabetic numeric "_")))))
  (identifier (:: alphabetic (:* (union alphabetic numeric "_")) (:? "'"))))

(define (reserved-keyword? str)
  (let ([reserved-keywords-lst
         '("BOOL" "INT" "NAT" "NAT1" "FALSE" "TRUE" "bool" "card" "dom" "finite" "id" "inter" "max" "min" "mod" "pred" "POW" "POW1" "prj1" "prj2" "ran" "succ" "union" "partition" "oftype" "status")])
    (if (memf (lambda (e) (string-ci=? str e))
              reserved-keywords-lst)
        #t #f)))

(define eb-lexer 
  (lexer-src-pos
   ;; Ignore Comments
   ((:or (:- whitespace #\newline) c-style-comment) (return-without-pos (eb-lexer input-port)))
   
   ((eof) 'EOF)
   ((:or line-comment #\newline) 'NEWLINE)
   
   ;; Predicate Calculus Tokens
   ("(" 'LEFTPAREN)
   (")" 'RIGHTPAREN)
   ("<=>" 'EQUIV)
   ("=>" 'IMPL)
   ("&" 'AND)
   ("or" 'OR)
   ("not" 'NOT)
   ("btrue" 'TOP)
   ("bfalse" 'BOTTOM)
   ("!" 'FORALL)
   ("#" 'EXISTS)
   ("." 'QDOT)
   
   ;; Tokens for expr->pred-calc operators
   ("=" 'EQ)
   ("/=" 'NOTEQ)
   ("<" 'LESS)
   (">" 'GREATER)
   ("<=" 'LEQ)
   (">=" 'GEQ)
   (":" 'IN)
   ("/:" 'NOTIN)
   ("<<:" 'SUBSET)
   ("/<<:" 'NOTSUBSET)
   ("<:" 'SUBSETEQ)
   ("/<:" 'NOTSUBSETEQ)
   
   ;; Tokens for building sets of relations and functions
   ("<->" 'REL)
   ("<<->" 'TREL)
   ("<->>" 'SREL)
   ("<<->>" 'STREL)
   ("+->" 'PFUN)
   ("-->" 'TFUN)
   (">+>" 'PINJ)
   (">->" 'TINJ)
   ("+>>" 'PSUR)
   ("->>" 'TSUR)
   (">->>" 'BIJ)
   
   ("{" 'LEFTCURLY)
   ("}" 'RIGHTCURLY)
   ("|->" 'MAPLET)
   ("{}" 'EMPTYSET)
   ("BUNION" 'BIGUNION)
   ("BINTER" 'BIGINTER)
   ("/\\" 'INTERSECTION)
   ("\\/" 'UNION)
   ("\\" 'SETMINUS)
   ("**" 'CARTPROD)
   
   ("[" 'LEFTSQ)
   ("]" 'RIGHTSQ)
   ("<+" 'RELOVL)
   ("circ" 'BCOMP)
   (";" 'FCOMP) 
   ("><" 'DIRPROD)
   ("||" 'PARPROD)
   ("~" 'TILDEOP)
   ("<<|" 'DOMSUB)
   ("<|" 'DOMRES)
   ("|>" 'RANRES)
   ("|>>" 'RANSUB)
   
   ("%" 'LAMBDA)
   ("inter" 'NARY-INTER)
   ("union" 'NARY-UNION)
   ("|" 'SUCHTHAT)
   
   ("," 'COMMA)
   (".." 'UPTO)
   ("+" 'PLUS)
   ("-" 'MINUS)
   ("*" 'MULT)
   ("/" 'DIV)
   ("^" 'EXP)
   
   ("::" 'IN-ASSIGN)
   (":|" 'SUCHTHAT-ASSIGN)
   (":=" 'ASSIGN)
   
   ("==" 'DEFI)
   ("status" 'STATUS)
   ("oftype" 'OFTYPE)
   ("POW" 'POWERSET)
   ("POW1" 'POWERSET1)
   ("BOOL" 'BOOL)
   ("bool" 'BOOLFN)
   ("prj1" 'PRJ1)
   ("prj2" 'PRJ2)
   ("FALSE" 'FALSE)
   ("card" 'CARD)
   ("max" 'MAX)
   ("TRUE" 'TRUE)
   ("dom" 'DOM)
   ("min" 'MIN)
   ("ran" 'RAN)
   ("finite" 'FINITE)
   ("partition" 'PARTITION)
   ("mod" 'MOD)
   ("succ" 'SUCC)
   ("id" 'ID)
   ("pred" 'PRED)
   
   ("end" 'END)
   ("End" 'MEND)
   ("Context" 'CONTEXT)
   ("Machine" 'MACHINE)
   ("Event" 'EVENT)
   ("Axioms" 'AXIOMS)
   ("Theorems" 'THEOREMS)
   ("Constants" 'CONSTANTS)
   ("Sets" 'SETS)
   ("Sees" 'SEES)
   ("Variables" 'VARIABLES)
   ("Invariants" 'INVARIANTS)
   ("Variants" 'VARIANTS)
   ("Extends" 'EXTENDS)
   ("Initialisation" 'INITIALISATION)
   
   ("convergent" 'CONVERGENT)
   ("normal" 'NORMAL)
   
   ("any" 'ANY)
   ("where" 'WHERE)
   ("when" 'WHEN)
   ("then" 'THEN)
   
   ("INT" 'INT)
   ("NAT" 'NAT)
   ("NAT1" 'NAT1)
   
   (label (token-LABEL (string->symbol (substring lexeme 1 (string-length lexeme)))))
   (int-literal (token-NUM (string->number lexeme)))
   (identifier (begin
                 (when (reserved-keyword? lexeme)
                   (error "You have used a reserved keyword as an identifier:" lexeme)) 
                 (token-IDENTIFIER lexeme)))
   
   (any-char
    (begin
      (error 'eb-lexer
             "Found lexeme that doesn't match any rule: ~a, in file ~a (~a,~a)-(~a,~a)."
             lexeme
             (current-file)
             (position-line start-pos)
             (position-col start-pos)
             (position-line end-pos)
             (position-col end-pos))))))

;                                            
;                                            
;                                            
;   ;;;;;                                    
;   ;   ;                                    
;   ;   ;;  ;;;    ; ;;;  ;;;    ;;;    ; ;;;
;   ;   ;      ;   ;;    ;;     ;;  ;   ;;   
;   ;;;;;      ;   ;     ;;     ;   ;   ;    
;   ;       ;;;;   ;      ;;;   ;;;;;   ;    
;   ;      ;   ;   ;         ;  ;       ;    
;   ;      ;   ;   ;         ;  ;       ;    
;   ;      ;;;;;   ;     ;;;;    ;;;;   ;    
;                                            
;                                            
;                                            


(define-values (machine-parser context-parser) 
  ((compose vector->values list->vector)
   (parser 
    (start machine context)
    (tokens pred-tokens expr->pred-tokens sets-relations-tokens set-manip-tokens rel-manip-tokens quant-expr-tokens arith-expr-tokens special-tokens ids-numbers-tokens str-tokens non-ascii-ids-tokens language-tokens)
    (error (lambda (tok-ok? tok-name tok-value start-pos end-pos)
             (raise-user-error 'parser
                               "Error parsing ~a~ntok-ok? ~a, tok-name ~a, tok-value ~a, start-pos (~a, ~a), end-pos (~a, ~a)." (current-file) tok-ok? tok-name tok-value (position-line start-pos) (position-col start-pos) (position-line end-pos) (position-col end-pos))))
    (precs 
     (left PLUS MINUS)
     (left MULT DIV MOD)
     (right UMINUS))
    (src-pos)
    (end EOF)
;    (debug "parser-debug.log")
    (grammar 
     
     ;; -> #<void>
     (newline-seq ((NEWLINE) (void))
                  ((newline-seq NEWLINE) (void)))
     
     ;; -> #<void>
     (newline-seq/opt (() (void))
                      ((newline-seq) (void)))
     
     ;; -> Machine
     (machine ((MACHINE IDENTIFIER newline-seq msecs-list MEND newline-seq/opt) 
               (let loop ([rest-msecs $4] 
                          [sees-list '()] 
                          [var-list '()] 
                          [event-list '()] 
                          [inv-list '()] 
                          [variant-list '()] 
                          [thm-list '()]
                          [init-list '()])
                 (if (null? rest-msecs)
                     (make-Machine $2 sees-list var-list event-list inv-list variant-list thm-list init-list)
                     (let ([symb (caar rest-msecs)]
                           [val (cdar rest-msecs)])
                       (case symb
                         [(sees) ;; This is for each sees section, which contains several contexts!
                          (loop (rest rest-msecs)
                                (append val sees-list)
                                var-list event-list inv-list variant-list thm-list init-list)]
                         [(var) ;; This is for each var section, which contains several variable definitions!
                          (loop (rest rest-msecs)
                                sees-list
                                (append val var-list)
                                event-list inv-list variant-list thm-list init-list)]
                         [(event) 
                          (loop (rest rest-msecs)
                                sees-list var-list
                                (cons val event-list)
                                inv-list variant-list thm-list init-list)]
                         [(inv) 
                          (loop (rest rest-msecs)
                                sees-list var-list event-list
                                (append val inv-list)
                                variant-list thm-list init-list)]
                         [(variant)
                          (loop (rest rest-msecs)
                                sees-list var-list event-list inv-list
                                (append val variant-list)
                                thm-list init-list)]
                         [(thm)
                          (loop (rest rest-msecs)
                                sees-list var-list event-list inv-list variant-list
                                (append val thm-list)
                                init-list)]
                         [(init)
                          (loop (rest rest-msecs)
                                sees-list var-list event-list inv-list variant-list thm-list
                                (append val init-list))]
                         [else
                          (error 'parser "BUG: During parsing of sections, unexpected symbol found: ~a." symb)]))))))
     
     ;; -> (Listof (Pair Symbol Any))
     (msecs-list ((sees-section)
                  (list (cons 'sees $1)))
                 ((var-section)
                  (list (cons 'var $1)))
                 ((event-section)
                  (list (cons 'event $1)))
                 ((inv-section)
                  (list (cons 'inv $1)))
                 ((variant-section)
                  (list (cons 'variant $1)))
                 ((thm-section)
                  (list (cons 'thm $1)))
                 ((init-section)
                  (list (cons 'init $1)))
                 ((msecs-list sees-section) 
                  (cons (cons 'sees $2) $1))
                 ((msecs-list var-section) 
                  (cons (cons 'var $2) $1))
                 ((msecs-list event-section) 
                  (cons (cons 'event $2) $1))
                 ((msecs-list inv-section) 
                  (cons (cons 'inv $2) $1))
                 ((msecs-list variant-section) 
                  (cons (cons 'variant $2) $1))
                 ((msecs-list thm-section) 
                  (cons (cons 'thm $2) $1))
                 ((msecs-list init-section) 
                  (cons (cons 'init $2) $1)))
     
     ;; -> (Listof Action)
     (init-section ((INITIALISATION DEFI newline-seq gsub-non-empty-list) $4))
     
     ;; -> (Listof Context)
     (sees-section ((SEES newline-seq identifier-non-empty-list newline-seq) 
                    ;; For each identifier in the sees operation, we parse the respective context.
                    (map (lambda (context-str)
                           (let ([context-filename (build-path (working-directory)
                                                               (string-append (symbol->string context-str) 
                                                                              ".ctx"))])
                             (when (not (file-exists? context-filename))
                               (error 'parser "Can't open context file ~a." context-filename))
                             (call-with-input-file context-filename
                               (lambda (ip)
                                 (port-count-lines! ip)
                                 (parameterize ([current-file context-filename])
                                   (context-parser (lambda () (eb-lexer ip)))))
                               #:mode 'text)))
                         $3)))
     
     ;; -> (Listof Symbol) 
     (identifier-non-empty-list ((IDENTIFIER) 
                                 (list (string->symbol $1)))
                                ((identifier-non-empty-list newline-seq/opt IDENTIFIER) 
                                 (cons (string->symbol $3) $1)))
     
     ;; -> (Listof Variable)
     (var-section ((VARIABLES newline-seq/opt identifier-non-empty-list newline-seq/opt)
                   (map make-Variable $3)))
     
     ;; -> Event
     (event-section ((EVENT IDENTIFIER DEFI newline-seq 
                            status-opt 
                            ANY newline-seq/opt identifier-non-empty-list newline-seq/opt
                            WHERE newline-seq label-opt-predicate-non-empty-list 
                            THEN newline-seq gsub-non-empty-list 
                            END newline-seq) 
                     (make-Event (string->symbol $2) $5 (map make-Variable $8) (reverse $12) (reverse $15)))
                    ((EVENT IDENTIFIER DEFI newline-seq 
                            status-opt 
                            WHEN newline-seq label-opt-predicate-non-empty-list 
                            THEN newline-seq gsub-non-empty-list 
                            END newline-seq)
                     (make-Event (string->symbol $2) $5 '() (reverse $8) (reverse $11))))
     
     ;; -> (U 'normal 'convergent)
     (status-opt (() 'normal)
                 ((STATUS CONVERGENT newline-seq) 'convergent)
                 ((STATUS NORMAL newline-seq) 'normal))
     
     ;; -> (Listof Labelled-Predicate)
     (label-opt-predicate-non-empty-list ((LABEL predicate newline-seq) 
                                          (list (make-Labelled $1 $2)))
                                         ((predicate newline-seq) 
                                          (list (make-Labelled/auto $1)))
                                         ((label-opt-predicate-non-empty-list LABEL predicate newline-seq) 
                                          (cons (make-Labelled $2 $3) $1))
                                         ((label-opt-predicate-non-empty-list predicate newline-seq)
                                          (cons (make-Labelled/auto $2) $1)))
     
     ;; -> (Listof Labelled-Action)
     (gsub-non-empty-list ((gsub newline-seq) 
                           (list $1))
                          ((gsub-non-empty-list gsub newline-seq) 
                           (cons $2 $1)))
     
     
     ;; -> Labelled-Action
     (gsub ((identifier-comma-non-empty-list SUCHTHAT-ASSIGN predicate)
            (make-Labelled/auto (make-Suchthat-Assign-Action (map (compose make-Identifier string->symbol) $1) $3)))
           ((LABEL identifier-comma-non-empty-list SUCHTHAT-ASSIGN predicate)
            (make-Labelled $1
                           (make-Suchthat-Assign-Action (map (compose make-Identifier string->symbol) $2) $4)))
           ((identifier-comma-non-empty-list ASSIGN expression-comma-non-empty-list)
            (make-Labelled/auto (make-Assign-Action (map (compose make-Identifier string->symbol) $1) $3)))
           ((LABEL identifier-comma-non-empty-list ASSIGN expression-comma-non-empty-list) 
            (make-Labelled $1
                           (make-Assign-Action (map (compose make-Identifier string->symbol) $2) $4)))
           ((IDENTIFIER LEFTPAREN expression RIGHTPAREN ASSIGN expression)
            (make-Labelled/auto 
             (make-Assign-Action (list (make-Identifier (string->symbol $1))) 
                                 (list 
                                  (make-Expression-BinOp 'ovl 
                                                         (make-Identifier (string->symbol $1)) 
                                                         (make-Set-Enumeration (list (make-Expression-BinOp 'mapsto $3 $6))))))))
           ((LABEL IDENTIFIER LEFTPAREN expression RIGHTPAREN ASSIGN expression)
            (make-Labelled $1 
                           (make-Assign-Action (list (make-Identifier (string->symbol $2))) 
                                               (list 
                                                (make-Expression-BinOp 'ovl 
                                                                       (make-Identifier (string->symbol $2)) 
                                                                       (make-Set-Enumeration (list (make-Expression-BinOp 'mapsto $4 $7))))))))
           ((IDENTIFIER IN-ASSIGN set-expr) 
            (make-Labelled/auto (make-In-Assign-Action (make-Identifier (string->symbol $1)) $3)))
           ((LABEL IDENTIFIER IN-ASSIGN set-expr) 
            (make-Labelled $1 (make-In-Assign-Action (make-Identifier (string->symbol $2)) $4))))
     
     ;; -> (Listof Symbol)
     (identifier-comma-non-empty-list ((IDENTIFIER) 
                                       (list $1))
                                      ((identifier-comma-non-empty-list COMMA IDENTIFIER) 
                                       (cons $3 $1)))
     
     ;; -> (Listof Labelled-Predicate)
     (inv-section ((INVARIANTS newline-seq label-opt-predicate-non-empty-list) 
                   $3))
     
     ;; -> (Listof Expression)
     (variant-section ((VARIANTS newline-seq expression-comma-non-empty-list) 
                       $3))
     
     ;; -> (Listof Labelled-Predicate)
     (thm-section ((THEOREMS newline-seq label-opt-predicate-non-empty-list) 
                   $3))
     
     ;; Predicate Grammar (taken from the EB documentation)
     
     ;; -> Predicate
     (predicate ((unquantified-predicate) 
                 $1)
                ((quantifier-non-empty-list unquantified-predicate) 
                 (foldr (lambda (pair pred) (make-Quantifier (car pair) (cdr pair) pred))
                        $2
                        $1)))
     
     ;; -> (Listof (Pair (U 'forall 'exists) Variable))
     (quantifier-non-empty-list ((quantifier) 
                                 $1)
                                ((quantifier-non-empty-list quantifier) 
                                 (append $2 $1)))
     
     ;; -> (Listof (Pair (U 'forall 'exists) Variable)) 
     (quantifier ((FORALL identifier-comma-non-empty-list QDOT) 
                  (map (lambda (v) (cons 'forall (make-Variable (string->symbol v)))) $2))
                 ((EXISTS identifier-comma-non-empty-list QDOT) 
                  (map (lambda (v) (cons 'exists (make-Variable (string->symbol v)))) $2)))
     
     ;; -> Predicate
     (unquantified-predicate ((simple-predicate) 
                              $1)
                             ((simple-predicate IMPL simple-predicate) 
                              (make-Predicate-BinOp 'limp $1 $3))
                             ((simple-predicate EQUIV simple-predicate)
                              (make-Predicate-BinOp 'leqv $1 $3)))
     
     ;; -> Predicate
     (simple-predicate ((literal-predicate) 
                        $1)
                       ((literal-predicate and-list)
                        (make-Predicate-BinOp 'land $1 $2))
                       ((literal-predicate or-list) 
                        (make-Predicate-BinOp 'lor $1 $2)))
     
     ;; -> (Listof Predicate)
     (and-list ((AND literal-predicate) $2)
               ((and-list AND literal-predicate) 
                (make-Predicate-BinOp 'land $1 $3)))
     
     ;; -> (Listof Predicate)
     (or-list ((OR literal-predicate) $2)
              ((or-list OR literal-predicate) 
               (make-Predicate-BinOp 'lor $1 $3)))
     
     ;; -> Predicate
     (literal-predicate ((atomic-predicate) $1)
                        ((not-list atomic-predicate)
                         (if (even? $1)
                             $2
                             (make-Predicate-UnOp 'not $2))))
     
     ;; -> Integer
     (not-list ((NOT) 1)
               ((not-list NOT) (+ $1 1)))
     
     ;; -> Predicate
     (atomic-predicate ((TOP) (make-Predicate-Literal 'btrue))
                       ((BOTTOM) (make-Predicate-Literal 'bfalse))
                       ((FINITE LEFTPAREN expression RIGHTPAREN) ;; A finite expression is always true in BOUNDED model checking TODO: I am not so sure it can be like this anymore since it's more than a bounded MC.
                        (make-Predicate-Literal 'btrue))
                       ((PARTITION LEFTPAREN expression-comma-non-empty-list RIGHTPAREN)
                        (make-Predicate-Partition (reverse $3)))
                       ((pair-expression relop pair-expression)
                        (make-Predicate-RelOp $2 $1 $3))
                       ((LEFTPAREN predicate RIGHTPAREN) $2))
     
     ;; -> Symbol
     (relop ((EQ) 'equal)
            ((NOTEQ) 'notequal)
            ((LESS) 'lt)
            ((GREATER) 'gt)
            ((LEQ) 'le)
            ((GEQ) 'ge)
            ((IN) 'in)
            ((NOTIN) 'notin)
            ((SUBSET) 'subset)
            ((NOTSUBSET) 'notsubset)
            ((SUBSETEQ) 'subseteq)
            ((NOTSUBSETEQ) 'notsubseteq))
     
     ;; Type Grammar
     (type ((basic-type cartprod-type-list)
            (foldl (lambda (t acum) (make-Type-CartesianProduct acum t))
                   $1
                   $2)))
     
     ;; -> Type
     (basic-type ((IDENTIFIER)
                  (make-Type-UndefEnumeration (string->symbol $1)))
                 ((INT) 
                  (make-Type-Integer))
                 ((BOOL) 
                  (make-Type-Boolean))
                 ((POWERSET LEFTPAREN type RIGHTPAREN)
                  (make-Type-Powerset $3))
                 ((LEFTPAREN type RIGHTPAREN)
                  $2))
     
     ;; -> (Listof Type)
     (cartprod-type-list (()
                          '())
                         ((cartprod-type-list CARTPROD basic-type) 
                          (cons $3 $1)))
     
     
     ;; Expression Grammar (taken from the EB documentation)
     
     
     (expression ((expression OFTYPE type) (make-Expr/wt $3 $1))
                 ((untyped-expression) $1))
     
     ;; -> Expression
     (untyped-expression ((LAMBDA ident-pattern QDOT predicate SUCHTHAT untyped-expression) 
                          (make-Lambda-Expression $2 $4 $6))
                         ((BIGUNION identifier-comma-non-empty-list QDOT predicate SUCHTHAT untyped-expression) 
                          (make-Set-Comprehension 'union $2 $4 $6))
                         ((BIGINTER identifier-comma-non-empty-list QDOT predicate SUCHTHAT untyped-expression) 
                          (make-Set-Comprehension 'inter $2 $4 $6))
                         ((pair-expression) $1))
     
     ;; -> Variable-Pattern
     (ident-pattern ((LEFTPAREN ident-pattern RIGHTPAREN MAPLET ident-pattern) 
                     (make-Variable-Pair $2 $5))
                    ((IDENTIFIER MAPLET ident-pattern)
                     (let ([var (make-Identifier (string->symbol $1))])
                       (make-Variable-Pair var $3)))
                    ((LEFTPAREN ident-pattern RIGHTPAREN) $2)
                    ((IDENTIFIER) (make-Identifier (string->symbol $1))))
     
     ;; -> Expression
     (pair-expression ((relation-set-expr relation-set-expr-maplet-list) 
                       (foldl (lambda (expr acum) (make-Expression-BinOp 'mapsto acum expr))
                              $1
                              $2)))
     
     ;; -> (Listof Expression)
     (relation-set-expr-maplet-list (() 
                                     '())
                                    ((relation-set-expr-maplet-list MAPLET relation-set-expr) 
                                     (cons $3 $1)))
     
     ;; -> Expression
     (relation-set-expr ((set-expr relation-set-op-list)
                         (foldl (lambda (pair acum)
                                  (make-Expression-BinOp (car pair) acum (cdr pair)))
                                $1
                                (reverse $2)))) ;; relation-set-op-list returns the list in reverse order
     
     
     ;; -> (Listof (Pair Symbol Expression))
     (relation-set-op-list (() 
                            '())
                           ((relation-set-op-list relation-set-op set-expr) 
                            (cons (cons $2 $3) $1)))
     
     ;; -> Symbol
     (relation-set-op ((REL)   'rel)
                      ((TREL)  'trel)
                      ((SREL)  'srel)
                      ((STREL) 'strel)
                      ((PFUN)  'pfun)
                      ((TFUN)  'tfun)
                      ((PINJ)  'pinj)
                      ((TINJ)  'tinj)
                      ((PSUR)  'psur)
                      ((TSUR)  'tsur)
                      ((BIJ)   'bij))
  
     (set-expr ((interval-expr union-non-empty-list) 
                (foldl (lambda (expr acum)
                         (make-Expression-BinOp 'bunion acum expr))
                         $1
                         $2))
               ((interval-expr cross-product-non-empty-list)
                (foldl (lambda (expr acum) 
                         (make-Expression-BinOp 'cprod acum expr))
                       $1
                       $2))
               ((interval-expr overload-non-empty-list) 
                (foldl (lambda (expr acum) 
                         (make-Expression-BinOp 'ovl acum expr))
                       $1 
                       $2))
               ((interval-expr bcomp-non-empty-list) 
                (foldl (lambda (expr acum) 
                         (make-Expression-BinOp 'bcomp acum expr))
                       $1 
                       $2))
               ((interval-expr PARPROD interval-expr) 
                (make-Expression-BinOp 'pprod $1 $3))
               ((interval-expr DOMRES relation-expr) 
                (make-Expression-BinOp 'domres $1 $3))
               ((interval-expr DOMSUB relation-expr) 
                (make-Expression-BinOp 'domsub $1 $3))
               ((relation-expr) $1))
     
     ;; -> (Listof Expression)
     (union-non-empty-list ((UNION interval-expr) (list $2))
                           ((union-non-empty-list UNION interval-expr) 
                            (cons $3 $1)))
     
     ;; -> (Listof Expression)
     (cross-product-non-empty-list ((CARTPROD interval-expr) (list $2))
                                   ((cross-product-non-empty-list CARTPROD interval-expr) 
                                    (cons $3 $1)))
     
     ;; -> (Listof Expression)
     (overload-non-empty-list ((RELOVL interval-expr) (list $2))
                              ((overload-non-empty-list RELOVL interval-expr)
                               (cons $3 $1)))
     
     ;; -> (Listof Expression)
     (bcomp-non-empty-list ((BCOMP interval-expr) (list $2))
                           ((bcomp-non-empty-list BCOMP interval-expr) 
                            (cons $3 $1)))
     
     (relation-expr ((interval-expr) $1) 
                    ((interval-expr DIRPROD interval-expr) 
                     (make-Expression-BinOp 'dprod $1 $3))
                    ((interval-expr RANRES interval-expr) 
                     (make-Expression-BinOp 'ranres $1 $3))
                    ((interval-expr RANSUB interval-expr) 
                     (make-Expression-BinOp 'ransub $1 $3))
                    ((interval-expr fcomp-non-empty-list) 
                     (foldl (lambda (expr acum) (make-Expression-BinOp 'fcomp acum expr))
                            $1
                            $2))
                    ((interval-expr fcomp-non-empty-list RANRES interval-expr) 
                     (let ([op1 (foldl (lambda (expr acum) (make-Expression-BinOp 'fcomp acum expr))
                                       $1
                                       $2)])
                       (make-Expression-BinOp 'ranres op1 $4)))
                    ((interval-expr fcomp-non-empty-list RANSUB interval-expr)
                     (let ([op1 (foldl (lambda (expr acum) (make-Expression-BinOp 'fcomp acum expr))
                                       $1
                                       $2)])
                       (make-Expression-BinOp 'ransub op1 $4)))
                    ((interval-expr inter-non-empty-list)
                     (foldl (lambda (expr acum) (make-Expression-BinOp 'binter acum expr)
                              $1
                              $2)))
                    ((interval-expr inter-non-empty-list RANRES interval-expr) 
                     (let ([op1 (foldl (lambda (expr acum) (make-Expression-BinOp 'binter acum expr))
                                       $1
                                       $2)])
                       (make-Expression-BinOp 'ranres op1 $4)))
                    ((interval-expr inter-non-empty-list RANSUB interval-expr)
                     (let ([op1 (foldl (lambda (expr acum) (make-Expression-BinOp 'binter acum expr))
                                       $1
                                       $2)])
                       (make-Expression-BinOp 'ransub op1 $4)))
                    ((interval-expr SETMINUS interval-expr)
                     (make-Expression-BinOp 'setminus $1 $3))
                    ((interval-expr inter-non-empty-list SETMINUS interval-expr)
                     (let ([op1 (foldl (lambda (expr acum) (make-Expression-BinOp 'binter acum expr))
                                       $1
                                       $2)])
                       (make-Expression-BinOp 'setminus op1 $4))))
     
     ;; -> (Listof Expression)
     (fcomp-non-empty-list ((FCOMP interval-expr) 
                            $2)
                           ((fcomp-non-empty-list FCOMP interval-expr) 
                            (cons $3 $1)))
     
     ;; -> (Listof Expression)
     (inter-non-empty-list ((INTERSECTION interval-expr) 
                            $2)
                           ((inter-non-empty-list INTERSECTION interval-expr) 
                            (cons $3 $1)))
     
     ;; -> Expression
     (interval-expr ((arithmetic-expr)
                     $1)
                    ((arithmetic-expr UPTO arithmetic-expr) 
                     (make-Expression-BinOp 'upto $1 $3)))
     
     ;; -> Expression
     (arithmetic-expr ((arithmetic-expr PLUS arithmetic-expr)
                       (make-Expression-BinOp 'plus $1 $3))
                      ((arithmetic-expr MINUS arithmetic-expr)
                       (make-Expression-BinOp 'minus $1 $3))
                      ((MINUS arithmetic-expr) (prec UMINUS) (make-Expression-UnOp 'uminus $2))
                      ((term) $1))
     
     ;; -> Expression
     (term ((term MULT term) 
            (make-Expression-BinOp 'mul $1 $3))
           ((term DIV term) 
            (make-Expression-BinOp 'div $1 $3))
           ((term MOD term) 
            (make-Expression-BinOp 'mod $1 $3))
           ((factor) $1))
     
     ;; -> Expression
     (factor ((image) $1)
             ((image EXP image) (make-Expression-BinOp 'expn $1 $3)))
     
     ;; -> Expression
     (image ((primary) $1)
            ((primary app-non-empty-list)
             (let ([ordered-list (reverse $2)])
               (foldl (lambda (pair acum)
                        (let ([op (car pair)]
                              [expr (cdr pair)])
                          (make-Expression-BinOp op acum expr)))
                      $1
                      ordered-list))))
     
     ;; -> (Listof (Pair Symbol Expression))
     (app-non-empty-list ((LEFTPAREN expression RIGHTPAREN) 
                          (list (cons 'funimage $2)))
                         ((LEFTSQ expression RIGHTSQ)
                          (list (cons 'relimage $2)))
                         ((app-non-empty-list LEFTPAREN expression RIGHTPAREN) 
                          (cons (cons 'funimage $3) $1))
                         ((app-non-empty-list LEFTSQ expression RIGHTSQ) 
                          (cons (cons 'relimage $3) $1)))
     
     ;; -> Expression
     (primary ((simple-expr) $1)
              ((simple-expr inverse-non-empty-list)
               (if (even? $2)
                   $1
                   (make-Expression-UnOp 'converse $1))))
     
     ;; -> Integer
     (inverse-non-empty-list ((TILDEOP) 1)
                             ((inverse-non-empty-list TILDEOP)
                              (+ $1 1)))
     
     ;; -> Expression
     (simple-expr ((BOOLFN LEFTPAREN predicate RIGHTPAREN) 
                   (make-Expression-Bool $3))
                  ((unary-op LEFTPAREN expression RIGHTPAREN) 
                   (make-Expression-UnOp $1 $3))
                  ((LEFTPAREN expression RIGHTPAREN) $2)
                  ((LEFTCURLY expression-comma-non-empty-list QDOT predicate SUCHTHAT expression RIGHTCURLY) 
                   ;; First element should be identifier-comma-non-empty-list
                   ;; but causes reduce/reduce conflict so this is a workaround to get it working.
                   (make-Set-Comprehension 'cset $2 $4 $6))
                  ((LEFTCURLY expression-comma-non-empty-list RIGHTCURLY) 
                   (make-Set-Enumeration $2))
                  ((INT) (make-Expression-Literal 'integer))
                  ((NAT) (make-Expression-Literal 'natural))
                  ((NAT1) (make-Expression-Literal 'natural1))
                  ((BOOL) (make-Expression-Literal 'bool))
                  ((TRUE) (make-Expression-Literal 'true))
                  ((FALSE) (make-Expression-Literal 'false))
                  ((EMPTYSET) (make-Expression-Literal 'emptyset))
                  ((SUCC) (make-Expression-Literal 'succ))
                  ((PRED) (make-Expression-Literal 'pred))
                  ((PRJ1) (make-Expression-Literal 'prj1))
                  ((PRJ2) (make-Expression-Literal 'prj2))
                  ((ID) (make-Expression-Literal 'bool))
                  ((IDENTIFIER) (make-Identifier (string->symbol $1)))
                  ((NUM) (make-Integer-Literal $1)))
     
     ;; -> (Listof Expression) in reverse
     (expression-comma-non-empty-list ((expression) (list $1))
                                      ((expression-comma-non-empty-list COMMA expression) 
                                       (cons $3 $1)))
     
     ;; -> Symbol
     (unary-op ((CARD) 'card)
               ((POWERSET) 'pow)
               ((POWERSET1) 'pow1)
               ((NARY-UNION) 'union)
               ((NARY-INTER) 'inter)
               ((DOM) 'dom)
               ((RAN) 'ran)
               ((MIN) 'min)
               ((MAX) 'max))
     
     ;; Context
     
     (context ((CONTEXT IDENTIFIER newline-seq csecs-list MEND newline-seq/opt) 
               (let loop ([rest-csecs $4] 
                          [ext-list '()]
                          [set-list '()]
                          [const-list '()]
                          [axiom-list '()]
                          [thm-list '()])
                 (if (null? rest-csecs)
                     (make-Context $2 ext-list set-list const-list axiom-list thm-list)
                     (let ([symb (caar rest-csecs)]
                           [val (cdar rest-csecs)])
                       (case symb
                         [(ext) 
                          (loop (rest rest-csecs)
                                (append val ext-list)
                                set-list const-list axiom-list thm-list)]
                         [(set)
                          (loop (rest rest-csecs)
                                ext-list
                                (append val set-list)
                                const-list axiom-list thm-list)]
                         [(const)
                          (loop (rest rest-csecs)
                                ext-list set-list
                                (append val const-list)
                                axiom-list thm-list)]
                         [(axiom) 
                          (loop (rest rest-csecs)
                                ext-list set-list const-list
                                (append val axiom-list)
                                thm-list)]
                         [(thm)
                          (loop (rest rest-csecs)
                                ext-list set-list const-list axiom-list
                                (append val thm-list))]
                         [else 
                          (error 'parser "BUG: During parsing of sections, unexpected symbol found: ~a." symb)]))))))
     
     ;; -> (Listof (Pair Symbol Any))
     (csecs-list ((ext-section)
                  (list (cons 'ext $1)))
                 ((set-section)
                  (list (cons 'set $1)))
                 ((const-section)
                  (list (cons 'const $1)))
                 ((axioms-section)
                  (list (cons 'axiom $1)))
                 ((thm-section)
                  (list (cons 'thm $1)))
                 ((csecs-list ext-section) 
                  (cons (cons 'ext $2) $1))
                 ((csecs-list set-section) 
                  (cons (cons 'set $2) $1))
                 ((csecs-list const-section) 
                  (cons (cons 'const $2) $1))
                 ((csecs-list axioms-section) 
                  (cons (cons 'axiom $2) $1))
                 ((csecs-list thm-section) 
                  (cons (cons 'thm $2) $1)))
     
     (ext-section ((EXTENDS newline-seq/opt identifier-non-empty-list newline-seq)
                   ;; For each identifier in the extends operation, we parse the respective context (like Sees in Machine).
                   (map (lambda (context-str)
                          (let ([context-filename (build-path (working-directory)
                                                              (string-append (symbol->string context-str) 
                                                                             ".ctx"))])
                            (when (not (file-exists? context-filename))
                              (error 'parser "Can't open context file ~a." context-filename))
                            (call-with-input-file context-filename
                              (lambda (ip)
                                (port-count-lines! ip)
                                (parameterize ([current-file context-filename])
                                  (context-parser (lambda () (eb-lexer ip)))))
                              #:mode 'text)))
                        $3)))
     
     ;; -> (Listof Set)
     (set-section ((SETS newline-seq settypes-non-empty-list)
                   $3))
     
     ;; -> (Listof Set)
     (settypes-non-empty-list ((settypes newline-seq)
                               (list $1))
                              ((settypes-non-empty-list settypes newline-seq) 
                               (cons $2 $1)))
     
     ;; -> Set
     (settypes ((IDENTIFIER) 
                (make-Set (string->symbol $1)))
               ((IDENTIFIER ASSIGN LEFTCURLY identifier-comma-non-empty-list RIGHTCURLY) 
                (let ([setlits (map (lambda (id) (make-Set-Literal id))
                                    (map string->symbol $4))])
                  (make-Enumerated-Set (string->symbol $1) setlits))))
     
     ;; -> (Listof symbol)
     ;; Each predicate defines a constant whose name is the same as the unique free variable of the predicate.
     (const-section ((CONSTANTS newline-seq/opt identifier-non-empty-list newline-seq)
                     (map make-Constant $3)))
     
     ;; -> (Listof Labelled-Predicate)
     (axioms-section ((AXIOMS newline-seq label-opt-predicate-non-empty-list)
                      $3))))))



;                                                                                                           
;                                                                                                           
;                                                                                         ;                 
;   ;;;;;                                            ;;;;;;                        ;                        
;   ;    ;                                           ;                             ;                        
;   ;    ;  ;;;    ; ;;   ;;;    ;;;    ; ;;         ;      ;   ;  ; ;;    ;;;   ;;;;;  ;;;     ;;;   ; ;;  
;   ;    ; ;   ;   ;;  ; ;   ;  ;;  ;   ;;  ;        ;      ;   ;  ;;  ;  ;;  ;    ;      ;    ;; ;;  ;;  ; 
;   ;;;;;      ;   ;     ;      ;   ;   ;            ;;;;;; ;   ;  ;   ;  ;        ;      ;    ;   ;  ;   ; 
;   ;       ;;;;   ;      ;;;   ;;;;;   ;            ;      ;   ;  ;   ;  ;        ;      ;    ;   ;  ;   ; 
;   ;      ;   ;   ;         ;  ;       ;            ;      ;   ;  ;   ;  ;        ;      ;    ;   ;  ;   ; 
;   ;      ;   ;   ;     ;   ;  ;;  ;   ;            ;      ;   ;  ;   ;  ;;  ;    ;      ;    ;; ;;  ;   ; 
;   ;       ;; ;   ;      ;;;    ;;;    ;            ;       ;; ;  ;   ;   ;;;     ;;;  ;;;;;   ;;;   ;   ; 
;                                                                                                           
;                                                                                                           
;                                                                                                           

(define (parse-machine file)
  (time-apply-if "Parsing" (verbose-mode)
                 (lambda ()
                   (when (not (file-exists? file))
                     (error 'parser "Can't open file ~a." file))
                   (call-with-input-file file
                     (lambda (ip)
                       (port-count-lines! ip)
                       (parameterize ([current-file file])
                         (machine-parser (lambda () (eb-lexer ip)))))
                     #:mode 'text))))