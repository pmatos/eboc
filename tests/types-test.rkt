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

;; types tests
(require (planet schematics/schemeunit:3:4/text-ui)
         (planet schematics/schemeunit:3:4/test)
         (only-in srfi/1 every)
         "../types.rkt")

;; Type definition
(define t0 (t '(P Z)))
(define t1 (t '(P (Z x a)))) 
(define t2 (t '(P (P ((a x b) x (Z x a)))))) 
(define t3 (t '(P (b x B))))

(run-tests
 (test-suite
  "types"
  
  (test-suite
   "t"
   (test-begin
    (check-true (type=? t0 (make-Type-Powerset (make-Type-Integer)))))
   
   (test-begin 
    (check-true (type=? t1 (make-Type-Powerset (make-Type-CartesianProduct (make-Type-Integer)
                                                                           (make-Type-Polymorphic 'a))))))
   
   (test-begin
    (check-true (type=? t2 (make-Type-Powerset (make-Type-Powerset (make-Type-CartesianProduct (make-Type-CartesianProduct (make-Type-Polymorphic 'a)
                                                                                                                           (make-Type-Polymorphic 'b))
                                                                                               (make-Type-CartesianProduct (make-Type-Integer)
                                                                                                                           (make-Type-Polymorphic 'a))))))))
   
   (test-begin
    (check-true (type=? t3 (make-Type-Powerset (make-Type-CartesianProduct (make-Type-Polymorphic 'b)
                                                                           (make-Type-Boolean)))))))
  
  (test-suite
   "unify-types"
   (letrec ([symbol<? 
             (lambda (s1 s2)
               (string<? (symbol->string s1) (symbol->string s2)))]
            [assignment=?
             (lambda (a1 a2)
               (and a1 a2 ;; making sure none is #f
                    (let ([a1-sorted (sort a1 symbol<? #:key (compose Type-Polymorphic-name car))]
                          [a2-sorted (sort a2 symbol<? #:key (compose Type-Polymorphic-name car))])
                      (every (lambda (p1 p2) (and (type=? (car p1) (car p2)) 
                                                  (type=? (cdr p1) (cdr p2))))
                             a1-sorted
                             a2-sorted))))])
     
     (test-begin 
      (check assignment=?
             (unify-types (list (cons (t 'a) (t 'Z))
                                (cons (t 'b) (t 'a))))
             `((,(t 'a) . ,(t 'Z)) (,(t 'b) . ,(t 'Z)))))
     
     (test-begin 
      (check-false
       (unify-types (list (cons (t 'a) (t 'Z))
                          (cons (t 'b) (t 'a))
                          (cons (t 'B) (t 'b))))))
     
     (test-begin
      (check assignment=?
             (unify-types (list (cons (t 'b) (t 'a))
                                (cons (t '(P Z)) (t '(P a)))
                                (cons (t 'Z) (t 'Z))
                                (cons (t 'b) (t 'Z))))
             `((,(t 'a) . ,(t 'Z)) (,(t 'b) . ,(t 'Z)))))
     
     (test-begin
      (check-false
       (unify-types (list (cons (t 'b) (t 'a))
                          (cons (t 'a) (t 'b))))))
     
     (test-begin
      (check assignment=?
       (unify-types (list (cons (t `(P a)) (t 'c))
                          (cons (t `(P b)) (t 'c))))
       `((,(t 'b) . ,(t 'a)) (,(t 'c) . ,(t '(P a))))))
     
     (test-begin
      (check assignment=?
             (unify-types (list (cons (t `(P a)) (t `(P b)))
                                (cons (t `(a x B)) (t `(Z x B)))
                                (cons (t `(b x c)) (t `(Z x B)))))
             `((,(t 'c) . ,(t 'B)) (,(t 'b) . ,(t 'Z)) (,(t 'a) . ,(t 'Z)))))
  
     (test-suite 
      "unify-types/uniquely"
      
      (test-begin
       (check assignment=?
              (unify-types (list (cons (t 'Z) (t 'a))
                                 (cons (t '(P Z)) (t '(P a)))))
              `((,(t 'a) . ,(t 'Z)))))
      
      (test-begin
       (check-false
        (unify-types/uniquely (list (cons (t `(P a)) (t 'c))
                                    (cons (t `(P b)) (t 'c)))))))))
  
  (test-suite
   "type->ground-type"
   (let ([subst `((,(t 'c) . ,(t 'B)) (,(t 'b) . ,(t 'Z)) (,(t 'a) . ,(t 'Z)))])
     (test-begin
      (check type=?
             (type->ground-type (t `(a x (P ((b x a) x c))))
                                subst)
             (t '(Z x (P ((Z x Z) x B))))))))))

