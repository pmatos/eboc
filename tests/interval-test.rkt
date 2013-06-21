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
         "../modes/esmc-mode/interval.rkt")

(run-tests
 (test-suite
  "intervals"
  
  (test-suite
   "int-minus"
   (test-case "half delete"
    (check-equal? (int-minus (make-int -inf.0 +inf.0) (make-int 0 +inf.0)) (list (make-int -inf.0 -1))))
   
   (test-case "border delete"
    (check-equal? (int-minus (make-int 0 4) (make-int 4 4)) (list (make-int 0 3))))
   
   (test-case "split"
    (check-equal? (int-minus (make-int -10 10) (make-int 0 1)) (list (make-int -10 -1) (make-int 2 10))))
   
   (test-case "redundant delete"
    (check-equal? (int-minus (make-int 0 10) (make-int 11 100)) (list (make-int 0 10))))
   
   (test-case "complete delete"
    (check-equal? (int-minus (make-int 0 10) (make-int -inf.0 +inf.0)) (list))))))
    