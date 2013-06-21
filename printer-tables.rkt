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

(require "params.rkt")

;                                                                               
;                                                                               
;          ;                                         ;      ;;;                 
;     ;;;  ;                          ;;;;;;;        ;        ;                 
;    ;   ; ;                             ;           ;        ;                 
;   ;      ; ;;    ;;;    ; ;;           ;     ;;;   ;;;;     ;     ;;;    ;;;  
;   ;      ;;  ;  ;   ;   ;;  ;          ;    ;   ;  ;; ;;    ;    ;;  ;  ;   ; 
;   ;      ;   ;      ;   ;              ;        ;  ;   ;    ;    ;   ;  ;     
;   ;      ;   ;   ;;;;   ;              ;     ;;;;  ;   ;    ;    ;;;;;   ;;;  
;   ;      ;   ;  ;   ;   ;              ;    ;   ;  ;   ;    ;    ;          ; 
;    ;   ; ;   ;  ;   ;   ;              ;    ;   ;  ;;  ;    ;    ;;  ;  ;   ; 
;     ;;;  ;   ;   ;; ;   ;              ;     ;; ;  ;;;;      ;;   ;;;    ;;;  
;                                                                               
;                                                                               
;                                                                               


(provide type-op-table
         quantifier-op-table
         predicate-literal-table
         predicate-unop-table
         predicate-binop-table
         predicate-relop-table
         expression-literal-table
         expression-binop-table
         expression-unop-table
         constant-characters-table)

(define type-op-table
  (make-parameter
   '((Z        . ("INT"   "\u2124"))
     (B        . ("BOOL"   ""))
     (P        . ("POW"   "\u2119"))
     (x        . ("**"   "\u00d7")))))

(define quantifier-op-table
  (make-parameter
   '((forall   . ("!"   "\u2200"))
     (exists   . ("#"   "\u2203")))))

(define predicate-literal-table
  (make-parameter
   '((btrue    . ("TOP"      ""))
     (bfalse   . ("BOTTOM"   "")))))

(define predicate-unop-table
  (make-parameter
   '((not      . ("not" "\u00ac")))))

(define predicate-binop-table
  (make-parameter
   '((land      . ("&"   "\u2227"))
     (lor       . ("or"  "\u2228"))
     (limp      . ("=>"  "\u21d2"))
     (leqv      . ("<=>" "\u21d4")))))

(define predicate-relop-table
  (make-parameter
   '((equal    . ("="   "\u003d"))
     (notequal . ("/=" "\u2260"))
     (lt        . ("<"   "\u003c"))
     (le       . ("<="  "\u2264"))
     (gt        . (">"   "\u003e"))
     (ge       . (">="  "\u2265"))
     (in       . (":"   "\u2208"))
     (notin      . ("/:"  "\u2209"))
     (subset   . ("<<:" "\u2282"))
     (notsubset  . ("/<<:" "\u2284"))
     (subseteq . ("<:"   "\u2286"))
     (notsubseteq . ("/<:"  "\u2288")))))

(define expression-literal-table
  (make-parameter 
   ;; Symbolic Name -> (Textual Representation | Unicode Representation)
   '((integer   . ("INT" "\u2124"))
     (natural   . ("NAT" ""))
     (natural1  . ("NAT1" ""))
     (pred      . ("pred" ""))
     (succ      . ("succ" ""))
     (bool      . ("bool" ""))
     (true      . ("TRUE" ""))
     (false     . ("FALSE" ""))
     (emptyset  . ("{}"    ""))
     (id        . ("id"    ""))
     (prj1      . ("prj1"  ""))
     (prj2      . ("prj2"  "")))))

(define expression-binop-table
  (make-parameter 
   ;; Symbolic Name -> (Textual Representation | Unicode Representation)
   '((mapsto   . ("|->"     "\u21a6"))
     (rel      . ("<->"     "\u2194"))
     (trel     . ("<<->"    "\ue100")) 
     (srel     . ("<->>"    "\ue101"))
     (strel    . ("<<->>"   "\ue102"))
     (pfun     . ("+>"      "\u21f8"))
     (tfun     . ("-->"     "\u2192"))
     (pinj     . (">+>"     "\u2914"))
     (tinj     . (">->"     "\u21a3"))
     (psur     . ("+>>"     "\u2900"))
     (tsur     . ("->>"     "\u21a0"))
     (bij      . (">->>"    "\u2916"))
     (bunion   . ("\\/"     "\u222a"))
     (cprod    . ("**"      "\u00d7"))
     (ovl      . ("<+"      "\ue103"))
     (bcomp    . ("circ"    "\u2218"))
     (pprod    . ("||"      "\u2225"))
     (domres   . ("<|"      "\u25c1"))
     (domsub   . ("<<|"     "\u2a64"))
     (dprod    . ("><"      "\u2297")) 
     (ranres   . ("|>"      "\u25b7"))
     (ransub   . ("|>>"     "\u2a65")) 
     (fcomp    . (";"       "\u003b"))
     (binter   . ("/\\"     "\u2229"))
     (setminus . ("\\"      "\u2216"))
     (upto     . (".."      "\u2025"))
     (plus     . ("+"       "\u002b"))
     (minus    . ("-"       "\u2212"))
     (mul      . ("*"       "\u2217"))
     (div      . ("div"     "\u00f7"))
     (mod      . ("mod"     "mod"))
     (expn     . ("^"       "\u005e"))
     (funimage . ("funimage" ""))
     (relimage . ("relimage" "")))))

(define expression-unop-table
  (make-parameter 
   ;; Symbolic Name -> (Textual Representation | Unicode Representation)
   '((converse . ("~"       ""))
     (uminus   . ("-"    ""))
     (card     . ("card"    ""))
     (pow      . ("POW"     ""))
     (pow1     . ("POW1"    ""))
     (union    . ("UNION"       ""))
     (inter    . ("INTER"       ""))
     (dom      . ("dom"     ""))
     (ran      . ("ran"     ""))
     (min      . ("min"     ""))
     (max      . ("max"     "")))))

(define constant-characters-table 
  (make-parameter
   '((qdot   . ("."        "\u00b7")))))


;                                                                 
;                                                                 
;                   ;    ;;;      ;             ;                 
;   ;    ;   ;             ;             ;                        
;   ;    ;   ;             ;             ;                        
;   ;    ; ;;;;;  ;;;      ;    ;;;    ;;;;;  ;;;     ;;;    ;;;  
;   ;    ;   ;      ;      ;      ;      ;      ;    ;;  ;  ;   ; 
;   ;    ;   ;      ;      ;      ;      ;      ;    ;   ;  ;     
;   ;    ;   ;      ;      ;      ;      ;      ;    ;;;;;   ;;;  
;   ;    ;   ;      ;      ;      ;      ;      ;    ;          ; 
;   ;    ;   ;      ;      ;      ;      ;      ;    ;;  ;  ;   ; 
;    ;;;;    ;;;  ;;;;;     ;;  ;;;;;    ;;;  ;;;;;   ;;;    ;;;  
;                                                                 
;                                                                 
;                                                                 


(define (table-seek table op proc)
  (let ([p (assoc op (table))])
    (if p
        (proc (cdr p))
        (error 'table-seek "Can't find operator ~a in operator table." op))))

(define (op-rep table op)
  (table-seek table op first))

(provide print-table-operation
         print-table-constant
         available-ops)

(define (available-ops table)
  (map first (table)))

;; Length of args should be the correct one.
;; No checking is done at this level.
(define print-table-operation 
  (case-lambda [(table op arg port)
                (fprintf port "~a(~a)" (op-rep table op) arg)]
               [(table op arg1 arg2 port)
                (fprintf port "(~a ~a ~a)" 
                         arg1 (op-rep table op) arg2)]))

(define (print-table-constant table const port)
  (fprintf port "~a" (op-rep table const)))