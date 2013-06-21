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

(provide listof?
         dict-merge)

(require (only-in srfi/1 every))

;; Returns true if lst is a list of elements accepted by proc.
;; So if proc is a predicate for integers, listof? tests if
;; lst is a list of integers.
(define (listof? proc u)
  (and (list? u)
       (every proc u)))

;; Merges several dicts assuming that the sets of keys are disjoint
(define dict-merge 
  (case-lambda [(dict) dict]
               [(dict1 dict2)
                (let loop ([res dict1] [next (dict-iterate-first dict2)])
                  (if next
                      (loop (dict-set res (dict-iterate-key dict2 next) (dict-iterate-value dict2 next))
                            (dict-iterate-next dict2 next))
                      res))]
               [(dict . dicts)
                (foldl (lambda (el acum) (dict-merge acum el))
                       dict
                       dicts)]))