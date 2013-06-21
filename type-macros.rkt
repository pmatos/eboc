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

(require (except-in "types.rkt" t))

(provide typing-rules)

;; Type Macros
(define-syntax t
  (syntax-rules (P x : Z B)
    [(_ Z) (make-Type-Integer)]
    [(_ B) (make-Type-Boolean)]
    [(_ (P a)) (make-Type-Powerset (t a))]
    [(_ (a x b)) (make-Type-CartesianProduct (t a) (t b))]
    [(_ (name : (el1 els ...))) (make-Type-Enumeration name (cons el1 els ...))]
    [(_ a) (make-Type-Polymorphic a)]))

(define-for-syntax (expand-rules stx)
  (syntax-case stx (rule)
    [(rule (ops ...) (vars ...) ((lhs == rhs) ...) return-type)
     #'((ops ...)
        (let ([vars (make-fresh-type-variable-name)] ...)
          (values 
           (map make-Type-Polymorphic (list vars ...))
           (list (cons lhs (t rhs)) ...)
           (t return-type))))]))

(define-syntax (typing-rules stx)
  (syntax-case stx ()
    [(_ name (types ...) rules ...)
     #'(identifier? name)
     (with-syntax ([(branch ...) (map expand-rules (syntax->list #'(rules ...)))])
       #'(define name
           (lambda (op types ...)
             (case op branch ...))))]))