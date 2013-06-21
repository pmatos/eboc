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

;; This mode implements composition of machines.
;; Given two typed machines, it composes them according to some rules
;; generating the corresponding proof obligations that allow the resulting
;; machine to be meaninful.

(require (only-in srfi/1 lset-intersection lset-union)
         "../ast.rkt"
         "../params.rkt"
         "../macro-utils.rkt"
         "../eventb-parser.rkt")

(provide composition-mode)

(define-struct args
  (name files)
  #:mutable)

(define (check-args mode-options)
  (let ([args (make-args "" '())])
    (let loop ([rest-opts mode-options])
      (cond  [(null? rest-opts) args]
             [(eq? (caar rest-opts) 'file)
              (set-args-files! args (cons (cdar rest-opts) (args-files args)))
              (loop (rest rest-opts))]
             [(eq? (caar rest-opts) 'name)
              (set-args-name! args (cdar rest-opts))
              (loop (rest rest-opts))]
             [else 
              (error 'check-args "Unknown option to composition mode: ~a" (caar rest-opts))]))))

;; Accepted mode options
;; file=filename - filename of a machine to compose (2 occurrences)
(define (composition-mode mode-options)
  (let* ([args (check-args mode-options)]
         [file1 (build-path (working-directory) (first (args-files args)))]
         [file2 (build-path (working-directory) (second (args-files args)))]
         [ast1 (parse-machine file1)]
         [ast2 (parse-machine file2)])
    (unless (and (Machine? ast1) (Machine? ast2))
      (error 'compose-mode "At the moment, only machines can be composed."))
    (printf "~n~nP.O.s of Machine Composition:~n")
    (printf "-----------------------------~n")
    (for-each (match-lambda ((cons p str) (printf "~a~n~a~n~n" str p)))
              (compose-machine/pos ast1 ast2))
    (printf "~n~nResulting Machine:~n")
    (printf "------------------~n")
    (printf "~a" (compose-machine (args-name args) ast1 ast2))))


;; Returns an assoc list where keys are predicates (the proof obligations)
;; and values are strings (the explanations)
(define (compose-machine/pos m1 m2 #:init-mode (init-mode 'eqv))
  (list (call-with-values (lambda () (compose-variables/pos (Machine-vars m1) (Machine-vars m2)))
                          (lambda (p str) (cons p (string-append "Variables Composition PO: " str))))
        (call-with-values (lambda () (compose-invariants/pos (Machine-invariants m1) (Machine-invariants m2)))
                          (lambda (p str) (cons p (string-append "Invariants Composition PO: " str))))
        (call-with-values (lambda () (compose-initialisations/pos (first (Machine-initialisation m1)) (first (Machine-initialisation m2)) #:mode init-mode))
                          (lambda (p str) (cons p (string-append "Initialisation Composition PO: " str))))))

(define (compose-machine name m1 m2 #:inv-mode (inv-mode 'conj) #:init-mode (init-mode 'conj))
  (make-Machine name
                '()
                (compose-variables (Machine-vars m1) (Machine-vars m2))
                '()
                (map make-Labelled/auto (compose-invariants (Machine-invariants m1) (Machine-invariants m2) #:mode inv-mode))
                '()
                '()
                (list (make-Labelled/auto (compose-initialisations (first (Machine-initialisation m1))  (first (Machine-initialisation m2)) #:mode init-mode)))))

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

;; v1, v2 : listof typed-variables
;; returns 2 values, a predicate to be proved and a string 
;; explaining the proof obligation
(define (compose-variables/pos v1 v2)
  (letrec ([find-type/list
            (lambda (vname lst)
              (let loop ([rest-lst lst])
                (cond [(null? rest-lst) #f]
                      [(eq? vname (Variable-name (Expr/wt-expr (first rest-lst))))
                       (Expr/wt-type (first rest-lst))]
                      [else (loop (rest rest-lst))])))]
           [make-var-po
            (lambda (v) 
              (p `(,(type->expression (find-type/list v v1)) equal ,(type->expression (find-type/list v v2)))))])
    (let* ([po/str "! x1, x2 . (x1 : v1 & x2 : v2 & x1 = x2) => t(x1) =t t(x2)"]
           [common-vars/name (lset-intersection eq?
                                                (map (compose Variable-name Expr/wt-expr) v1)
                                                (map (compose Variable-name Expr/wt-expr) v2))])
      (values
       (if (null? common-vars/name)
           (p 'btrue)
           (foldl (lambda (v pred) (p `(,pred land ,(make-var-po v))))
                  (make-var-po (first common-vars/name))
                  (rest common-vars/name)))
       po/str))))

(define (compose-variables v1 v2)
  (lset-union variable/typed=? v1 v2))


;                                                                        
;                                                                        
;                                        ;                               
;   ;;;;;                                                     ;          
;     ;                                                       ;          
;     ;    ; ;;   ;   ;   ;;;    ; ;;  ;;;     ;;;   ; ;;   ;;;;;   ;;;  
;     ;    ;;  ;  ;   ;  ;   ;   ;;  ;   ;    ;   ;  ;;  ;    ;    ;   ; 
;     ;    ;   ;   ; ;       ;   ;       ;        ;  ;   ;    ;    ;     
;     ;    ;   ;   ; ;    ;;;;   ;       ;     ;;;;  ;   ;    ;     ;;;  
;     ;    ;   ;   ; ;   ;   ;   ;       ;    ;   ;  ;   ;    ;        ; 
;     ;    ;   ;    ;    ;   ;   ;       ;    ;   ;  ;   ;    ;    ;   ; 
;   ;;;;;  ;   ;    ;     ;; ;   ;     ;;;;;   ;; ;  ;   ;    ;;;   ;;;  
;                                                                        
;                                                                        
;                                                                    ; ; 

(define (compose-invariants/pos inv1 inv2)
  (let* ([po/str "# v1, v2 . (&& inv1(v1 /\\ v2)) & (&& inv2(v1 /\\ v2))"]
         [unlabelled-inv1 (map Labelled-struct inv1)]
         [unlabelled-inv2 (map Labelled-struct inv2)]
         [free-vars/inv1 (apply lset-union variable/typed=? (map free-vars/predicate unlabelled-inv1))]
         [free-vars/inv2 (apply lset-union variable/typed=? (map free-vars/predicate unlabelled-inv2))]
         [free-vars/common (lset-intersection variable/typed=? free-vars/inv1 free-vars/inv2)]
         [inv1/with-common (filter (lambda (inv) (not (null? (lset-intersection variable/typed=? 
                                                                                (free-vars/predicate inv)
                                                                                free-vars/common))))
                                   unlabelled-inv1)]
         [inv2/with-common (filter (lambda (inv) (not (null? (lset-intersection variable/typed=?
                                                                                (free-vars/predicate inv)
                                                                                free-vars/common))))
                                   unlabelled-inv2)])
    (values 
     (if (and (null? inv1/with-common) (null? inv2/with-common))
         (p 'btrue)
         (let* ([conj-inv1 (if (null? inv1/with-common) 
                               (p 'btrue)
                               (foldl (lambda (el pred) (p `(,pred land ,el)))
                                      (first inv1/with-common)
                                      (rest inv1/with-common)))]
                [conj-inv2 (if (null? inv2/with-common)
                               (p 'btrue)
                               (foldl (lambda (el pred) (p `(,pred land ,el)))
                                      (first inv2/with-common)
                                      (rest inv2/with-common)))]
                [free-vars (lset-union variable/typed=? (free-vars/predicate conj-inv1) (free-vars/predicate conj-inv2))])
           (foldl (lambda (v pred) (make-Quantifier 'exists v pred))
                  (p `(,conj-inv1 land ,conj-inv2))
                  free-vars)))
     po/str)))

(define (compose-invariants inv1 inv2 #:mode (mode 'conj)) ;; mode can also be disj
  (let ([unlabelled-inv1 (map Labelled-struct inv1)]
        [unlabelled-inv2 (map Labelled-struct inv2)])
    (case mode
      [(conj) (append unlabelled-inv1 unlabelled-inv2)]
      [(disj) (list 
               (p `(,(foldl (lambda (inv pred) (p `(,pred land ,inv))) (first unlabelled-inv1) (rest unlabelled-inv1))
                    lor
                    ,(foldl (lambda (inv pred) (p `(,pred land ,inv))) (first unlabelled-inv2) (rest unlabelled-inv2)))))]
      [else (error 'compose-invariants "Unknown composition mode: ~a" mode)])))



;                                                                                                    
;                                                                                                    
;                   ;             ;           ;;;      ;                           ;                 
;   ;;;;;                  ;                    ;                           ;                        
;     ;                    ;                    ;                           ;                        
;     ;    ; ;;   ;;;    ;;;;;  ;;;     ;;;     ;    ;;;     ;;;    ;;;   ;;;;;  ;;;     ;;;   ; ;;  
;     ;    ;;  ;    ;      ;      ;    ;   ;    ;      ;    ;   ;  ;   ;    ;      ;    ;; ;;  ;;  ; 
;     ;    ;   ;    ;      ;      ;        ;    ;      ;    ;          ;    ;      ;    ;   ;  ;   ; 
;     ;    ;   ;    ;      ;      ;     ;;;;    ;      ;     ;;;    ;;;;    ;      ;    ;   ;  ;   ; 
;     ;    ;   ;    ;      ;      ;    ;   ;    ;      ;        ;  ;   ;    ;      ;    ;   ;  ;   ; 
;     ;    ;   ;    ;      ;      ;    ;   ;    ;      ;    ;   ;  ;   ;    ;      ;    ;; ;;  ;   ; 
;   ;;;;;  ;   ;  ;;;;;    ;;;  ;;;;;   ;; ;     ;;  ;;;;;   ;;;    ;; ;    ;;;  ;;;;;   ;;;   ;   ; 
;                                                                                                    
;                                                                                                    
;                                                                                                    

(define (compose-initialisations/pos init1 init2 #:mode (mode 'eqv)) ; mode can also be inter
  (let ([unlabelled-init1 (Labelled-struct init1)]
        [unlabelled-init2 (Labelled-struct init2)])
    (case mode
      [(eqv) 
       (let ([po/str "! v1', v2' . P(v1') <=> P(v2')"])
         (values 
          (foldl (lambda (v pred) (make-Quantifier 'forall v pred))
                 (p `(,(Suchthat-Assign-Action-rhs unlabelled-init1) leqv ,(Suchthat-Assign-Action-rhs unlabelled-init2)))
                 (lset-union variable/typed=? (Suchthat-Assign-Action-lhs unlabelled-init1) (Suchthat-Assign-Action-lhs unlabelled-init2)))
          po/str))]
      [(inter)
       (let ([po/str "# v1', v2' . P(v1') & P(v2')"])
         (values 
          (foldl (lambda (v pred) (make-Quantifier 'exists v pred))
                 (p `(,(Suchthat-Assign-Action-rhs unlabelled-init1) land ,(Suchthat-Assign-Action-rhs unlabelled-init2)))
                 (lset-union variable/typed=? (Suchthat-Assign-Action-lhs unlabelled-init1) (Suchthat-Assign-Action-lhs unlabelled-init2)))
          po/str))]
      [else (error 'compose-invariants/pos "Unknown composition mode: ~a" mode)])))

(define (compose-initialisations init1 init2 #:mode (mode 'conj)) ; mode can also be disj
  (let ([unlabelled-init1 (Labelled-struct init1)]
        [unlabelled-init2 (Labelled-struct init2)])
    (case mode
      [(conj)
       (make-Suchthat-Assign-Action 
        (lset-union variable/typed=? (Suchthat-Assign-Action-lhs unlabelled-init1) (Suchthat-Assign-Action-lhs unlabelled-init2))
        (p `(,(Suchthat-Assign-Action-rhs unlabelled-init1) land ,(Suchthat-Assign-Action-rhs unlabelled-init2))))]
      [(disj)
       (let ([common-vars (lset-intersection variable/typed=? (Suchthat-Assign-Action-lhs unlabelled-init1) (Suchthat-Assign-Action-lhs unlabelled-init2))]
             [init1/spliced (splice/predicate (Suchthat-Assign-Action-rhs unlabelled-init1))]
             [init2/spliced (splice/predicate (Suchthat-Assign-Action-rhs unlabelled-init2))])
         (let-values ([(init1/with-common init1/without-common)
                       (partition (lambda (p) (not (null? (lset-intersection variable/typed=? (map post-variable->variable (free-vars/predicate p)) common-vars)))) init1/spliced)]
                      [(init2/with-common init2/without-common)
                       (partition (lambda (p) (not (null? (lset-intersection variable/typed=? (map post-variable->variable (free-vars/predicate p)) common-vars)))) init2/spliced)])
           (make-Suchthat-Assign-Action 
            (lset-union variable/typed=? (Suchthat-Assign-Action-lhs init1) (Suchthat-Assign-Action-lhs init2))
            (join/predicate (if (null? init1/without-common) (p 'btrue) (apply join/predicate init1/without-common))
                            (if (null? init2/without-common) (p 'btrue) (apply join/predicate init2/without-common))
                            (p `(,(if (null? init1/with-common) (p 'bfalse) (apply join/predicate init1/with-common))
                                 lor
                                 ,(if (null? init2/with-common) (p 'bfalse) (apply join/predicate init2/with-common))))))))]
      [else (error 'compose-invariants "Unknown composition mode: ~a" mode)])))




