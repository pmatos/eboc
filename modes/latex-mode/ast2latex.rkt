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

;; This module provides conversion of an AST to LaTeX.
(require scheme/match
         mzlib/compat
         "../../ast.rkt"
         "../../types.rkt")

(define spaces/tab (make-parameter 2))

(define (print-tabs tablevel (out (current-output-port)))
  (fprintf out (make-string (* tablevel (spaces/tab)) #\space)))


;                                     
;                                     
;                                     
;  ;;;;;;;                            
;     ;                               
;     ;    ;   ;; ;;;;    ;;;    ;;;  
;     ;    ;   ;  ;;  ;  ;;  ;  ;;    
;     ;    ;;  ;  ;   ;  ;   ;  ;;    
;     ;     ; ;   ;   ;  ;;;;;   ;;;  
;     ;     ; ;   ;   ;  ;          ; 
;     ;      ;;   ;;  ;  ;          ; 
;     ;      ;    ;;;;    ;;;;  ;;;;  
;            ;    ;                   
;            ;    ;                   
;          ;;     ;                   

(define (type->latex type (out (current-output-port)))
  (parameterize ([current-output-port out])
    (match type
      ((struct Type-Integer _) (printf "\\intg"))
      ((struct Type-Boolean _) (printf "\\Bool"))
      ((struct Type-Polymorphic (name)) (printf "~a" name))
      ((struct Type-UndefEnumeration (name)) (printf "~a" name))
      ((struct Type-CartesianProduct (subtype1 subtype2)) (type->latex subtype1) (printf " \\cprod ~a") (type->latex subtype2))
      ((struct Type-Powerset (subtype)) (printf "\\pow(") (type->latex subtype) (printf ")")))))
                 


;                                                                                             
;                                                                                             
;                                                                                             
;   ;;;;;                    ;                 ;;;          ;;;;;                             
;   ;   ;                    ;                ;;            ;                                 
;   ;   ;;  ; ;;;  ;;;    ;;;;   ;;;          ;;            ;      ;   ;  ;;;;    ; ;;;  ;;;  
;   ;   ;   ;;    ;;  ;  ;  ;;  ;;             ;            ;       ; ;   ;;  ;   ;;    ;;    
;   ;;;;;   ;     ;   ;  ;   ;  ;;            ; ;  ;        ;;;;;   ;;;   ;   ;   ;     ;;    
;   ;       ;     ;;;;;  ;   ;   ;;;         ;  ;; ;        ;        ;    ;   ;   ;      ;;;  
;   ;       ;     ;      ;   ;      ;        ;   ;;;        ;       ;;;   ;   ;   ;         ; 
;   ;       ;     ;      ;   ;      ;         ;   ;         ;      ;; ;;  ;;  ;   ;         ; 
;   ;       ;      ;;;;   ;;;;  ;;;;           ;;;;;        ;;;;;  ;   ;  ;;;;    ;     ;;;;  
;                                                                         ;                   
;                                                                         ;                   
;                                                                         ;            ;      

(define (expression->latex expr (out (current-output-port)))
  (parameterize ([current-output-port out])
    (match expr
      ((struct Expr/wt (type expr))
       (expression->latex expr))
      ((struct Expression-Literal (lit)) 
       (case lit
         [(integer)   (printf "\\intg")]
         [(natural)   (printf "\\nat")]
         [(natural1)  (printf "\\natn")]
         [(pred)      (printf "\\upred")]
         [(succ)      (printf "\\usucc")]
         [(bool)      (printf "\\Bool")]
         [(true)      (printf "\\True")]
         [(false)     (printf "\\False")]
         [(emptyset)  (printf "\\emptyset")]
         [(id)        (printf "\\id")]
         [(prj1)      (printf "\\prj1")]
         [(prj2)      (printf "\\prj2")]
         [else (error 'expression->latex "Unexpected Expression Literal (~a), conversion failed." lit)]))
      ((struct Integer-Literal (val)) (printf "~a" val))
      ((struct Variable (name)) (printf "~a" name))
      ((struct Variable-Pair (car cdr))
       (expression->latex car)
       (printf " \\mapsto ")
       (expression->latex cdr))
      ((struct Expression-Bool (pred)) 
       (printf "\\bool(") (predicate->latex pred) (printf ")"))
      ((struct Expression-UnOp (op arg))
       (case op
         [(converse) (printf "(") (expression->latex arg) (printf ")^{-1}")]
         [(uminus) (printf "-(") (expression->latex arg) (printf ")")]
         [(card) (printf "\\card(") (expression->latex arg) (printf ")")]
         [(pow) (printf "\\pow(") (expression->latex arg) (printf ")")]
         [(pow1) (printf "\\pown(") (expression->latex arg) (printf ")")]
         [(union) (printf "\\union") (expression->latex arg)]
         [(dom) (printf "\\dom(") (expression->latex arg) (printf ")")]
         [(ran) (printf "\\ran(") (expression->latex arg) (printf ")")]
         [(min) (printf "\\min(") (expression->latex arg) (printf ")")]
         [(max) (printf "\\max(") (expression->latex arg) (printf ")")]
         [else (error 'expression->latex "Unexpected Unary operator (~a), conversion failed." op)]))
      ((struct Expression-BinOp (op arg1 arg2))
       (case op
         [(funimage)
          (expression->latex arg1)
          (printf "(")
          (expression->latex arg2)
          (printf ")")]
         [(relimage)
          (expression->latex arg1)
          (printf "[")
          (expression->latex arg2)
          (printf "]")]
         [else
          (expression->latex arg1)
          (printf " \\~a " op)
          (expression->latex arg2)]))
      ((struct Lambda-Expression (id-pat pred expr))
       (printf "\\lambda ")
       (expression->latex id-pat)
       (printf " \\qdot ")
       (predicate->latex pred)
       (printf " \\mid ")
       (expression->latex expr))
      ((struct Set-Comprehension (comp-op vars pred expr))
       (case comp-op
         [(union) (printf "\\Union (")]
         [(inter) (printf "\\Inter (")]
         [(none) (printf "\\{")]
         [else (error 'expression->latex "Unexpected Set Comprehension operator (~a), conversion failed." comp-op)])
       (let loop ([rest-vars vars])
         (if (null? (rest rest-vars))
             (printf "~a" (Variable-name (first rest-vars)))
             (begin 
               (printf "~a, " (Variable-name (first rest-vars)))
               (loop (rest rest-vars)))))
       (printf " \\qdot ")
       (predicate->latex pred)
       (printf " \\mid ")
       (expression->latex expr)
       (case comp-op ;; Valid comp-op checked before so no need to have an else clause
         [(union inter) (printf ")")]
         [(none) (printf "\\}")]))
      ((struct Set-Enumeration (exprs))
       (printf "\\{")
       (let loop ([rest-exprs exprs])
         (if (null? (rest rest-exprs))
             (expression->latex (first rest-exprs))
             (begin 
               (expression->latex (first rest-exprs))
               (printf ", ")
               (loop (rest rest-exprs)))))
       (printf "\\}")))))
               
                     
(define (predicate->latex pred (out (current-output-port)))
  (parameterize ([current-output-port out])
    (match pred
      ((struct Predicate-Literal (lit))
       (case lit
         [(btrue) (printf "\\btrue")]
         [(bfalse) (printf "\\bfalse")]
         [else (error 'predicate->latex "Unexpected Predicate Literal (~a), conversion failed." lit)]))
      ((struct Quantifier (quant var pred))
       (case quant
         [(forall) (printf "\\forall")]
         [(exists) (printf "\\exists")]
         [else (error 'predicate->latex "Unexpected Quantifier (~a), conversion failed." quant)]))
      ((struct Predicate-UnOp (op arg))
       (case op
         [(not) (printf "\\lnot")]
         [else (error 'predicate->latex "Unexpected Predicate Unary operator (~a), conversion failed." op)])
       (printf "(")
       (predicate->latex arg)
       (printf ")"))
      ((struct Predicate-BinOp (op arg1 arg2))
       (predicate->latex arg1)
       (printf " \\~a " op)
       (predicate->latex arg2))
      ((struct Predicate-RelOp (op arg1 arg2))
       (expression->latex arg1)
       (printf "\\~a " op)
       (expression->latex arg2)))))

(define (typed-variable->latex tv (out (current-output-port)))
  (match tv
    ((struct Expr/wt (type (struct Variable (name))))
     (printf "~a \\in " name) (type->latex type out))))
     

;                                            
;                                            
;                                            
;   ;;;;;                        ;;          
;   ;                            ;;          
;   ;      ;   ;   ;;;   ;;;;   ;;;;;   ;;;  
;   ;      ;   ;  ;;  ;  ;;  ;   ;;    ;;    
;   ;;;;;  ;   ;  ;   ;  ;   ;   ;;    ;;    
;   ;       ; ;   ;;;;;  ;   ;   ;;     ;;;  
;   ;       ; ;   ;      ;   ;    ;        ; 
;   ;       ;;;   ;      ;   ;    ;        ; 
;   ;;;;;    ;     ;;;;  ;   ;    ;;;  ;;;;  
;                                            
;                                            
;                                            


(define (event->latex ev (out (current-output-port)) #:tablevel (ntabs 0))
  (parameterize ([current-output-port out])
    (match ev 
      ((struct Event (name status locals guards actions))
       (print-tabs ntabs) (printf "Event ~a $\\defi$~n" name)
    
       (unless (eq? status 'normal)
         (print-tabs (1+ ntabs)) (printf "status ~a~n" status))
       
       (unless (null? locals)
         (print-tabs (1+ ntabs)) (printf "any~n")
         (for-each (lambda (v) 
                     (print-tabs (+ 2 ntabs)) (typed-variable->latex v))
                   locals))
       
       (print-tabs (1+ ntabs)) (if (null? locals) (printf "when~n") (printf "where~n"))
       
       (for-each (lambda (g) 
                   (print-tabs (+ 2 ntabs))
                   (labelled->latex g)
                   (newline))
                 guards)
       
       (print-tabs (1+ ntabs)) (printf "then~n")
       
       (for-each (lambda (a)
                   (print-tabs (+ 2 ntabs))
                   (labelled->latex a)
                   (newline))
                 actions)
       
       (print-tabs (1+ ntabs)) (printf "end~n")))))
                 

(define (action->latex a (out (current-output-port)))
  (parameterize ([current-output-port out])
    (match a 
      ((struct Assign-Action (vars exprs)) 
       (printf "~a" (apply string-append (add-between (map (compose symbol->string Variable-name) vars) ", ")))
       (printf " \\bcmeq ")
       (for-each expression->latex exprs))
      ((struct In-Assign-Action (var set-expr))
       (printf "~a" (Variable-name var))
       (printf " \\bcmin ")
       (expression->latex set-expr))
      ((struct Suchthat-Assign-Action (vars pred))
       (printf "~a" (apply string-append (add-between (map Variable-name vars) ", ")))
       (printf " \\bcmsuch ")
       (predicate->latex pred)))))
  

;                                                          
;                                                          
;                                                          
;  ;;   ;;               ;        ;                        
;  ;;;  ;;               ;                                 
;  ;;; ;;;  ;;;     ;;;  ; ;;    ;;    ;;;;    ;;;    ;;;  
;  ;;; ; ;     ;   ;     ;;  ;    ;    ;;  ;  ;;  ;  ;;    
;  ;; ;  ;     ;  ;      ;   ;    ;    ;   ;  ;   ;  ;;    
;  ;; ;  ;  ;;;;  ;      ;   ;    ;    ;   ;  ;;;;;   ;;;  
;  ;;    ; ;   ;  ;      ;   ;    ;    ;   ;  ;          ; 
;  ;;    ; ;   ;   ;     ;   ;    ;    ;   ;  ;          ; 
;  ;;    ; ;;;;;    ;;;  ;   ;  ;;;;;  ;   ;   ;;;;  ;;;;  
;                                                          
;                                                          
;                                                          
(provide machine->latex)

(define (machine->latex mch (out (current-output-port)) #:tablevel (ntabs 0))
  (parameterize ([current-output-port out])
    (match mch
      ((struct Machine (name sees vars events invs variants thms inits))
       (printf "Machine ~a~n~n" name)
       
       (unless (null? sees)
         (print-tabs (1+ ntabs)) (printf "Sees~n")
         (for-each (lambda (cname) (print-tabs (+ 2 ntabs)) (printf "~a~n" cname)) (map Context-name sees)))
       
       (unless (null? vars)
         (print-tabs (1+ ntabs)) (printf "Variables~n")
         (for-each (lambda (tv) (print-tabs (+ 2 ntabs)) (typed-variable->latex tv)) vars))
       
       (unless (null? invs)
         (print-tabs (1+ ntabs)) (printf "Invariants~n")
         (for-each (lambda (inv) (print-tabs (+ 2 ntabs)) (labelled->latex inv) (newline)) inits))
       
       (unless (null? inits)
         (print-tabs (1+ ntabs)) (printf "Initialisation~n")
         (for-each (lambda (init) (print-tabs (+ 2 ntabs)) (labelled->latex init) (newline)) inits))
       
       (unless (null? events)
         (for-each (lambda (ev) (event->latex ev #:tablevel (+ ntabs 1)) (newline)) events))
       
       (unless (null? thms)
         (print-tabs (1+ ntabs)) (printf "Theorems~n~n")
         (for-each (lambda (thm) (print-tabs (+ 2 ntabs)) (labelled->latex thm) (newline)) inits))
       
       (unless (null? variants)
         (print-tabs (1+ ntabs)) (printf "Variants~n~n")
         (for-each (lambda (var) (print-tabs (+ 2 ntabs)) (expression->latex var) (newline)) variants))
       
       (print-tabs ntabs) (printf "End~n")))))

(define (labelled->latex l (out (current-output-port)))
  (parameterize ([current-output-port out])
    (match l
      ((struct Labelled (label val)) 
       (unless (Labelled/auto? l)
         (printf "[~a] " label))
       (if (action? val)
           (action->latex val)
           (predicate->latex val))))))
       

;                                                   
;                                                   
;                                                   
;     ;;;                 ;;                   ;;   
;    ;                    ;;                   ;;   
;   ;       ;;;   ;;;;   ;;;;;   ;;;   ;   ;  ;;;;; 
;   ;      ;   ;  ;;  ;   ;;    ;;  ;   ; ;    ;;   
;   ;      ;   ;  ;   ;   ;;    ;   ;   ;;;    ;;   
;   ;      ;   ;  ;   ;   ;;    ;;;;;    ;     ;;   
;   ;      ;   ;  ;   ;    ;    ;       ;;;     ;   
;    ;     ;   ;  ;   ;    ;    ;      ;; ;;    ;   
;     ;;;   ;;;   ;   ;    ;;;   ;;;;  ;   ;    ;;; 
;                                                   
;                                                   
;                                                   

(define (context->latex ctx (out (current-output-port)) #:tablevel (ntabs 0))
  (parameterize ([current-output-port out])
    (match ctx
      ((struct Context (name exts sets consts axioms thms))
       (print-tabs ntabs) (printf "Context ~a~n~n" name)
       
       (unless (null? exts)
         (print-tabs (1+ ntabs)) (printf "Extends~n") 
         (for-each (lambda (ctxname) (print-tabs (+ 2 ntabs)) (printf "~a~n")) exts))
       
       (unless (null? sets)
         (print-tabs (1+ ntabs)) (printf "Sets~n")
         (for-each (lambda (s) (print-tabs (+ 2 ntabs)) (set->latex s) (newline)) sets))
       
       (unless (null? consts) 
         (print-tabs (1+ ntabs)) (printf "Constants~n")
         (for-each (lambda (c) (print-tabs (+ 2 ntabs)) (predicate->latex c) (newline)) consts))
       
       (unless (null? axioms)
         (print-tabs (1+ ntabs)) (printf "Axioms~n")
         (for-each (lambda (a) (print-tabs (+ 2 ntabs)) (predicate->latex a) (newline)) axioms))
       
       (unless (null? thms) 
         (print-tabs (1+ ntabs)) (printf "Theorems~n")
         (for-each (lambda (t) (print-tabs (+ 2 ntabs)) (predicate->latex t) (newline)) thms))
       
       (print-tabs ntabs) (printf "End~n")))))

(define (set->latex s (out (current-output-port)))
  (parameterize ([current-output-port out])
    (match s 
      ((struct Set (name)) (printf "~a" name))
      ((struct Enumerated-Set (name vals)) 
       (printf "~a \\bcmeq \\{~a}"
               name
               (apply string-append (add-between (map symbol->string vals) ", ")))))))