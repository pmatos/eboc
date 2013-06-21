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

(require "predexpr.rkt"
         "action.rkt"
         "../params.rkt"
         srfi/13)
        
(define-struct Labelled
  (label struct)
  #:property prop:custom-write 
  (lambda (struct port write?)
    (pp-Labelled struct port)))

(define (pp-Labelled struct (port (current-output-port)))
  (when (or (print-debug?) (not (auto-label? (Labelled-label struct))))
    (fprintf port "@~a " (Labelled-label struct)))
  (fprintf port "~a" (Labelled-struct struct)))

(define (labelled-predicate? u)
  (and (Labelled? u)
       (predicate? (Labelled-struct u))))

(define (labelled-typed-predicate? u)
  (and (Labelled? u)
       (typed-predicate? (Labelled-struct u))))

(define (labelled-action? u)
  (and (Labelled? u)
       (action? (Labelled-struct u))))

(define (make-Labelled/auto val) (make-Labelled (make-auto-label) val))

(define (make-auto-label) (gensym 'auto-label))

(define (auto-label? s)
  (string-prefix? "auto-label" (symbol->string s)))

(define (Labelled/auto? l)
  (auto-label? (Labelled-label l)))

(provide/contract
 [struct Labelled ((label symbol?) (struct (or/c predicate? action?)))]
 [labelled-predicate? (any/c . -> . boolean?)]
 [labelled-typed-predicate? (any/c . -> . boolean?)]
 [labelled-action? (any/c . -> . boolean?)]
 [make-Labelled/auto ((or/c predicate? action?) . -> . Labelled?)]
 [Labelled/auto? (Labelled? . -> . boolean?)])