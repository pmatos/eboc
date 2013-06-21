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

(require (only-in srfi/1 lset-union lset-difference)
         (prefix-in set: (planet soegaard/galore:4:1/set))
         "ast.rkt"
         "types.rkt"
         "environment.rkt")

(provide (rename-out (pass-machine machine/simplify)))

;; Simplifications in Machine:
;; - All the contexts are merged into one
;; - All UndefEnumeration types will be replaced by Enumerations

;; We may simplify actions in two different ways. This is passed as an option to pass-machine.
;; init-simplify? simplifies initialisation by transforming everything into non-deterministic assignments
;; ini-compact? compacts initialisation by having all the initialisation rules only as 2 (one deterministic and one non-deterministic)
(define (pass-machine mach 
                      #:defset-bound (ds-bound #f) 
                      #:action-simplify simp-act ; simp-act can be 'det or 'ndet
                      #:init-simplify? initsimp?
                      #:init-compact? init-compact?)
  (match mach
    ((struct Machine (name sees vars events invariants variants theorems inits))
     (let* ([unbounded-ctx (if (null? sees) (make-empty-context) (apply context-merge sees))]
            [ctx (pass-context unbounded-ctx ds-bound)])
       (make-Machine name
                     (list ctx)
                     vars
                     (map (lambda (e)
                            (case simp-act
                              [(det) (pass-event/det e ctx)]
                              [(ndet) (pass-event/nondet e ctx)]
                              [else (error 'pass-machine "Simplification of machine failed, simplify action mode unknown: " simp-act)]))
                          events)
                     (map (lambda (p)
                            (make-Labelled (Labelled-label p)
                                           (pass-predicate (Labelled-struct p))))
                          invariants)
                     (map pass-expression variants)
                     (map (lambda (p)
                            (make-Labelled (Labelled-label p)
                                           (pass-predicate (Labelled-struct p))))
                          theorems)
                     (cond [init-compact? 
                            (map make-Labelled/auto (compact-actions (map Labelled-struct inits)))]
                           [initsimp?
                            (map (lambda (a) 
                                   (make-Labelled (Labelled-label a)
                                                  (pass-action (Labelled-struct a))))
                                 inits)]
                           [else inits]))))))

;; Simplifications in Event through pass-event/nondet:
;; - guards have their labels removed, recursively simplified and conjoined;
;; -- note that we couldn't conjoin the guard without removing the labels.
;; - actions have their labels removed and are transformed into one before-after 
;;   predicate containing all state variables;
(define (pass-event/nondet ev ctx)
  (match ev
    ((struct Event (name status locals-list guards-list actions-list))
     (make-Event name
                 status
                 (map (match-lambda ;; TODO : We already did the same thing above for the machine variables... REFACTOR! ABSTRACT!
                        ((struct Expr/wt (type var))
                         (make-Expr/wt (undef-type->type type (get-enumerated-sets-as-types/assoc ctx)) var)))
                      locals-list)
                 (let ([passed-guards (map (compose pass-predicate Labelled-struct) guards-list)])
                   (if (= (length passed-guards) 1)
                       passed-guards
                       (list (foldl (lambda (el acum) (p `(,acum land ,el)))
                                    (first passed-guards)
                                    (rest passed-guards)))))
                 (list (transform-into-before-after-predicate
                        (map (compose pass-action Labelled-struct) actions-list)))))))

;; Simplifications in Event through pass-event/det:
;; - guards have their labels removed, recursively simplified and conjoined;
;; -- note that we couldn't conjoin the guard without removing the labels.
;; - actions have their labels removed and are transformed into one deterministic action
;;   by moving all non determinism into the guards.
;; This makes this pass slightly more complex than pass-event/nondet because, the pass-action
;; has to introduce more locals and guards, which didn't happen in pass-event/nondet.
;; Another note is that this pass doesn't create an action that sets all variables. Sets
;; only the ones explicitly set by the event.
(define (pass-event/det ev ctx)
  (letrec ([transform-into-deterministic 
            ;; Receives a list of actions and returns 3 values
            ;; - action : a single deterministic action assigning to all 
            ;;   the variables assigned in the list of actions received.
            ;; - new-locals : new introduced local variables due to non-determinism removal.
            ;; - new-guards : new introduced guards due to non-determinism removal.
            (lambda (actions)
              (let loop ([rest-actions actions] 
                         [assignments-lhs '()]
                         [assignments-rhs '()]
                         [newlocals '()] 
                         [newguards '()])
                (if (null? rest-actions)
                    (values 
                     (make-Assign-Action assignments-lhs assignments-rhs)
                     newlocals
                     newguards)
                    (match (first rest-actions)
                      ((struct Assign-Action (lhs rhs))
                       (loop (rest rest-actions)
                             (append lhs assignments-lhs)
                             (append rhs assignments-rhs)
                             newlocals
                             newguards))
                      ((struct In-Assign-Action (var set-expr))
                       (let* ([newvar (make-Variable (gensym (Variable-name var)))]
                              [newguard (p `(,newvar in ,set-expr))])
                         (loop (rest rest-actions)
                               (cons var assignments-lhs)
                               (cons newvar assignments-rhs)
                               (cons newvar newlocals)
                               (cons newguard newguards))))
                      ((struct Suchthat-Assign-Action (vars pred))
                       (let* ([action-locals (map (compose make-Variable gensym Variable-name) vars)])
                         (loop (rest rest-actions)
                               (append vars assignments-lhs)
                               (append action-locals assignments-rhs)
                               (append action-locals newlocals)
                               (cons (subst-in-predicate pred (map cons (map variable->post-variable vars) action-locals))
                                     newguards))))))))])     
    (match ev
      ((struct Event (name status locals-list guards-list actions-list))
       (let-values ([(action new-locals new-guards) (transform-into-deterministic (map Labelled-struct actions-list))])
         (make-Event name
                     status
                     (append locals-list new-locals)
                     (map make-Labelled/auto ;; The result of conjoining all the guards is a single non-labelled guard, we have to label it automatically
                          (let ([passed-guards (map (compose pass-predicate Labelled-struct) (append guards-list new-guards))])
                            (if (= (length passed-guards) 1)
                                passed-guards
                                (list (foldl (lambda (el acum) (p `(,acum land ,el)))
                                             (first passed-guards)
                                             (rest passed-guards))))))
                     (list (make-Labelled/auto action))))))))


;; Simplifications in Context:
;; - Removes partitions that represent enumerated sets from the axioms list and 
;;   makes them enumerated sets
(define (pass-context ctx ds-bound)
  (match ctx
    ((struct Context (name extensions sets constants axioms theorems))
     (let-values ([(new-axioms new-enum-sets consts) (partitions->enumerated-sets axioms)])
       (let ([context-before-dsbound (make-Context name
                                                   (map (lambda (c) (pass-context c ds-bound)) extensions)
                                                   (append new-enum-sets 
                                                           (remove* (map (compose make-Set Set-name) new-enum-sets) sets set=?))
                                                   (remove* consts constants constant=?)
                                                   (map pass-lpredicate new-axioms)
                                                   (map pass-lpredicate theorems))])
         (if ds-bound
             (bound-deferred-sets context-before-dsbound ds-bound)
             context-before-dsbound))))))

;; Filters a list of axioms for partitions and removes the partitions axioms
;; by replacing those that can be converted into enumerated sets by enumerated sets.
;; returns 2 values, an axiom list which is a subset of axioms and an enumerated set list
(define (partitions->enumerated-sets axioms)
  (let loop ([filtered-axioms '()]
             [enum-sets '()]
             [consts '()]
             [rest-axioms axioms])
    (if (null? rest-axioms) 
        (values filtered-axioms enum-sets consts)
        (match (first rest-axioms)
          ((struct Labelled (_
                             (struct Predicate-Partition ((list (struct Set (setname))
                                                                (struct Set-Enumeration ((list cs))) ...)))))
           (loop filtered-axioms
                 (cons (make-Enumerated-Set setname (map (compose make-Set-Literal Constant-name) cs)) enum-sets)
                 (append cs consts)
                 (rest rest-axioms)))
          (else 
           (loop (cons (first rest-axioms) filtered-axioms)
                 enum-sets
                 consts
                 (rest rest-axioms)))))))


(define (pass-lpredicate lpred)
  (match lpred
    ((struct Labelled (label pred))
     (make-Labelled label (pass-predicate pred)))))

;; Simplifications in Predicate:
;; Some predicates are simplified. The list follows and they are exactly the ones having 
;; specific match rules. (the rules noted with recurse means that the recursion will be made 
;; not only over P1/E1 or P2/E2 but over the whole expression)
;;             Top ==> ! Bottom
;; Exists x . P(x) ==> ! Forall x . ! P(x)
;;        P1 => P2 ==> ! P1 or P2
;;       P1 <=> P2 ==> P1 => P2 and P2 => P1 [recurse]
;; E1 <= E2 ==> E1 < E2 or E1 = E2
;; E1 > E2 ==> ! (E1 <= E2) [recurse]
;; E1 >= E1 ==> ! (E1 < E2) 
;; E1 != E2 ==> ! (E1 = E2)
;; E1 subseteq E2 ==> E1 in POW(E2)
;; E1 subset E2 ==> E1 subseteq E2 and E1 != E2 [recurse]
;; E1 !in E2 ==> ! (E1 in E2)
;; E1 !subseteq E2 ==> ! (E1 subseteq E2)
;; E1 !subset E2 ==> ! (E1 subset E2)
(define (pass-predicate pred)
  (match pred
    ((struct Predicate-Literal ('btrue))
     (pass-predicate (p `(not bfalse))))
    ((struct Quantifier ('forall var body))
     (pass-predicate (p `(not (exists ,var (not ,body))))))
    ((struct Predicate-BinOp ('limp arg1 arg2))
     (pass-predicate (p `((not ,arg1) lor ,arg2))))
    ((struct Predicate-BinOp ('leqv arg1 arg2))
     (pass-predicate (p `((,arg1 limp ,arg2) land (,arg2 limp ,arg1)))))
    ((struct Predicate-RelOp ('le arg1 arg2))
     (pass-predicate (p `((,arg1 lt ,arg2) lor (,arg1 equal ,arg2)))))
    ((struct Predicate-RelOp ('gt arg1 arg2))
     (pass-predicate (p `(not (,arg1 le ,arg2)))))
    ((struct Predicate-RelOp ('ge arg1 arg2))
     (pass-predicate (p `(not (,arg1 lt ,arg2)))))
    ((struct Predicate-RelOp ('notequal arg1 arg2))
     (pass-predicate (p `(not (,arg1 equal ,arg2)))))
    ((struct Predicate-RelOp ('subseteq arg1 arg2))
     (pass-predicate (p `(,arg1 in (pow ,arg2)))))
    ((struct Predicate-RelOp ('subset arg1 arg2))
     (pass-predicate (p `((,arg1 subseteq ,arg2) land (,arg1 notequal ,arg2)))))
    ((struct Predicate-RelOp ('notin arg1 arg2))
     (pass-predicate (p `(not (,arg1 in ,arg2)))))
    ((struct Predicate-RelOp ('notsubset arg1 arg2))
     (pass-predicate (p `(not (,arg1 subset ,arg2)))))
    ((struct Predicate-RelOp ('notsubseteq arg1 arg2))
     (pass-predicate (p `(not (,arg1 subseteq ,arg2)))))
    
    ;; DEFAULT
    ((struct Quantifier (quant var body))
     (make-Quantifier quant var (pass-predicate body)))
    ((struct Predicate-UnOp (op arg))
     (make-Predicate-UnOp op (pass-predicate arg)))
    ((struct Predicate-BinOp (op arg1 arg2))
     (make-Predicate-BinOp op (pass-predicate arg1) (pass-predicate arg2)))
    ((struct Predicate-RelOp (op arg1 arg2))
     (make-Predicate-RelOp op (pass-expression arg1) (pass-expression arg2)))
    ((struct Predicate-Literal _)
     pred)))

;; Simplifications in Expression:
;; Some operators are simplified. The list follows and they are exactly the ones having 
;; specific match rules.
;; POW1(E) ==> POW(E) \ {}
;; 
(define (pass-expression expression)
  (match expression
    ((struct Expression-UnOp ('pow1 s))
     (pass-expression (e `((pow ,e) setminus emptyset))))
    ((struct Expression-UnOp ('ran r))
     (pass-expression (e `(converse (dom ,r)))))
    
    ((struct Expression-BinOp ('mod n m))
     (pass-expression (e `(,n minus (,m mul (,n div ,m))))))
    ((struct Expression-BinOp ('rel s1 s2))
     (pass-expression (e `(pow (,s1 cprod ,s2)))))
;    ((struct Expression-BinOp ('strel s1 s2))
;     (pass-expression (e `((,s1 srel ,s2) binter (,s1 trel ,s2)))))
;    ((struct Expression-BinOp ('tinj s1 s2))
;     (pass-expression (e `((,s1 pinj ,s2) binter (,s1 tfun ,s2)))))
;    ((struct Expression-BinOp ('tsur s1 s2))
;     (pass-expression (e `((,s1 psur ,s2) binter (,s1 tfun ,s2)))))
;    ((struct Expression-BinOp ('bij s1 s2))
;     (pass-expression (e `((,s1 tinj ,s2) binter (,s1 tsur ,s2)))))
    ((struct Expression-BinOp ('bcomp r1 r2))
     (pass-expression (e `(,r2 fcomp ,r1))))
    ((struct Expression-BinOp ('domres s r))
     (pass-expression (e `((id ,s) fcomp ,r))))
    ((struct Expression-BinOp ('ranres r s))
     (pass-expression (e `(,r fcomp (id ,s)))))
    ((struct Expression-BinOp ('domsub s r))
     (pass-expression (e `(((dom ,r) setminus ,s) domres ,r))))
    ((struct Expression-BinOp ('ransub r s))
     (pass-expression (e `(,r ranres ((ran ,r) setminus ,s)))))
    ((struct Expression-BinOp ('ovl r1 r2))
     (pass-expression (e `(((dom ,r2) domsub ,r1) bunion ,r2))))
    ((struct Expression-BinOp ('relimage r s))
     (pass-expression (e `(ran (,s domres ,r)))))
    
    ;; DEFAULT 
    ((or (struct Expression-Literal _)
         (struct Integer-Literal _)
         (struct Variable _)
         (struct Constant _)
         (struct Set-Literal _)
         (struct Set _))
     expression)
    ((struct Expression-UnOp (op arg))
     (make-Expression-UnOp op (pass-expression arg)))
    ((struct Expression-BinOp (op arg1 arg2))
     (make-Expression-BinOp op (pass-expression arg1) (pass-expression arg2)))
    ((struct Lambda-Expression (id-pattern pred expr))
     (make-Lambda-Expression id-pattern (pass-predicate pred) (pass-expression expr)))
    ((struct Set-Comprehension (op vars pred expr))
     (make-Set-Comprehension op vars (pass-predicate pred) (pass-expression expr)))
    ((struct Set-Enumeration (exprs))
     (make-Set-Enumeration (map pass-expression exprs)))
    (_
     (error 'pass-expression/simplify "Unexpected expression : ~a" expression))))

;; Given an action, it transforms the action into a 
;; such that action using a before-after predicate.
(define (pass-action act)
  (match act
    ((struct Assign-Action (var-list expr-list))
     (make-Suchthat-Assign-Action var-list
                                  (let ([eq-list (map (lambda (var expr) (p `(,(variable->post-variable var) equal ,expr))) var-list expr-list)])
                                    (if (= (length eq-list) 1)
                                        (first eq-list)
                                        (foldl (lambda (el acum) (p `(,acum land ,el)))
                                               (first eq-list)
                                               (rest eq-list))))))
    ((struct In-Assign-Action (var set-expr))
     (make-Suchthat-Assign-Action (list var)
                                  (p `(,(variable->post-variable var) in ,set-expr))))
    ((struct Suchthat-Assign-Action _) act)))

;; Compact actions receives a list of unlabelled actions and returns:
;; a list with two elements, a deterministic action and a non-deterministic action
;; that corresponds to the deterministic actions and non-deterministic actions in the list
(define (compact-actions acts)
  (let loop ([det-lhs '()] [det-rhs '()] [ndet-lhs '()] [ndet-rhs '()] [rest-acts acts])
    (if (null? rest-acts)
        (append 
         (if (null? det-lhs) '() (list (make-Assign-Action det-lhs det-rhs)))
         (if (null? ndet-lhs) '() (list (make-Suchthat-Assign-Action ndet-lhs 
                                                                     (foldl (lambda (p acum) (p `(,acum land ,p))) 
                                                                            (first ndet-rhs)
                                                                            (rest ndet-rhs))))))
        (match (first rest-acts)
          ((struct Assign-Action (lhs rhs)) 
           (loop (append det-lhs lhs) (append det-rhs rhs) 
                 ndet-lhs ndet-rhs (rest rest-acts)))
          ((struct In-Assign-Action (var expr))
           (loop det-lhs det-rhs
                 (lset-union variable=? var ndet-lhs)
                 (cons (p `(,(variable->post-variable var) in ,expr)) ndet-rhs)
                 (rest rest-acts)))
          ((struct Suchthat-Assign-Action (vars pred))
           (loop det-lhs det-rhs
                 (lset-union variable=? vars ndet-lhs)
                 (cons pred ndet-rhs)
                 (rest rest-acts)))))))

;; Given a list of actions and a list of state variables, it transforms the actions into
;; a single such that action.
(define (transform-into-before-after-predicate act-list)
  (let* ([such-that-list (map pass-action act-list)] ;; list of actions as such-that
         [vars-set (append-map Suchthat-Assign-Action-lhs such-that-list)]) ;; list of variables set in the actions
    
    ;; Now, we grab all such-that-list and join it into one big such-that-action
    ;; the resulting rhs is the conjunction of all the rhs
    ;; the end lhs is the lsit of state variables. remember that all state variables need to be assigned.
    (make-Suchthat-Assign-Action
     vars-set
     (let loop ([such-that-rhs-list (map Suchthat-Assign-Action-rhs such-that-list)])
       (foldl (lambda (el acum) (p `(,acum land ,el)))
              (first such-that-rhs-list)
              (rest such-that-rhs-list))))))