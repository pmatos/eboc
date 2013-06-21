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

(require "../ast.rkt"
         "prop.rkt")

;; This is the main entry point for the SAT-based encoding
;; of predicates and expressions and the operational semantics
;; behind Event-B

(define-struct SAT-Predicate
  (constraints bvars pred))

(define-struct SAT-Expression
  (constraints bvars result expr))

(provide/contract
 [encode-predicate (predicate? . -> . SAT-Predicate)]
 [encode-expression (expression? . -> . SAT-Expression)]
 
 [struct SAT-Predicate ((constraints (listof Prop-Predicate?))
                        (bvars (listof Prop-Variable?))
                        (pred predicate?))]
 [struct SAT-Expression ((constraints (listof Prop-Predicate?))
                         (bvars (listof Prop-Variable?))
                         (expr expression?))])
