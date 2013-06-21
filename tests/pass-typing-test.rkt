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

(require (planet schematics/schemeunit:3:4/text-ui)
         (planet schematics/schemeunit:3:4/test)
         (only-in srfi/1 every)
         "../ast/predexpr.rkt")

(require/export "../pass-typing.rkt"
                (pass-predicate))

(run-tests
 (test-suite
  "pass-typing"
  
  (test-suite
   "typing-algorithm"
   
   (test-begin
    (let ([pred (p `((x in Z) land (1 le x)))])
      (let-values ([(vars env eqs typed-pred)
                    (pass-predicate pred '() (make-empty-env Variable? variable=?) '())])
        (check predicate=?
               (make-Predicate-BinOp)
               (retype-predicate typed-pred vars env eqs)))))
   
   (test-begin
    (let ([pred (p `(emptyset equal emptyset))])
      (let-values ([(vars env eqs typed-pred)
                    (pass-predicate pred '() (make-empty-env Variable? variable=?) '())])
        (check-exn exn?
                   (lambda ()
                     (retype-predicate typed-pred vars env eqs)))))))))
                        