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

(require "../../types.rkt"
         "enumerations.rkt"
         "probabilities.rkt")

;; This module builds on value enumerations to enumerate values of any eventb type in sequence.
;; For example: POW(Z), POW(B * Z), a, POW(a * Z) , etc where a is an enumerated set.
(define (type-enumeration t)
  (match t
    [(struct Type-Integer ()) int-enumeration]
    [(struct Type-Boolean ()) (gen-finite-enumeration '(true false))]
    [(struct Type-Enumeration (_ els)) (gen-finite-enumeration els)]
    [(struct Type-CartesianProduct (subtype1 subtype2))
     (let ([tenum1 (type-enumeration subtype1)]
           [tenum2 (type-enumeration subtype2)]
           [penum (gen-pair-enumeration (type-cardinality subtype1) (type-cardinality subtype2))])
       (lambda (n)
         (match-let ([(cons n1 n2) (penum n)])
           (cons (tenum1 n1) (tenum2 n2)))))]
    [(struct Type-Powerset (subtype))
     (let ([tenum (type-enumeration subtype)])
       (lambda (n)
         (map tenum (set-enumeration n))))]))

(define (type-cardinality t)
  (match t
    [(struct Type-Integer ()) +inf.0]
    [(struct Type-Boolean ()) 2]
    [(struct Type-Enumeration (_ els)) (length els)]
    [(struct Type-CartesianProduct (subtype1 subtype2))
     (* (type-cardinality subtype1) (type-cardinality subtype2))]
    [(struct Type-Powerset (subtype))
     (expt 2 (type-cardinality subtype))]))

(define (type-enumeration/prt t)
  (match t
    [(struct Type-Boolean ()) (lambda (n) 1.0)]
    [(struct Type-Enumeration (_ _)) (lambda (n) 1.0)]
    [else prob]))

(define (type-list-enumerator types)
  (let* ([type-enumerations (map type-enumeration types)]
         [type-enumerations/prt (map type-enumeration/prt types)]
         [enum (natural-list-enumerator (map type-cardinality types))]
         [next-enum (enum)])
    (case-lambda [() 
                  (if (not next-enum)
                      #f
                      (let ([next-enum-lst (map (lambda (t-enum val) (t-enum val)) type-enumerations next-enum)])
                        (begin0 
                          next-enum-lst
                          (set! next-enum (enum)))))]
                 [(msg)
                  (case msg
                    [(prt) 
                     (if next-enum
                         (foldl * 1.0 (map (lambda (prob-enum val) (prob-enum val)) type-enumerations/prt next-enum))
                         0)]
                    [else (error 'type-list-enumerator "Can't understand message : ~a" msg)])])))

(provide type-list-enumerator)