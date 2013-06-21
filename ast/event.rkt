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
         "labelled.rkt"
         "action.rkt"
         "../params.rkt"
         "../untyped-utils.rkt")

(provide pp-Event)

(define-struct Event 
  (name status locals guards actions)
  #:property prop:custom-write
  (lambda (struct port write?)
    (pp-Event struct port)))

(define (pp-Event struct (port (current-output-port)) #:tab-level (tabs 0))
  (parameterize ([current-output-port port])
    (letrec ([indent (lambda (n) (printf (make-string (* (spaces/tab) n) #\space)))])
      (match struct
        ((struct Event (name status locals guards actions))
         (indent tabs) (printf "Event ~a~n" name)
      
         (unless (eq? status 'normal)
           (indent (+ 1 tabs)) (printf "status ~a~n" status))
         
         (unless (null? locals)
           (indent (+ 1 tabs)) (printf "any~n")
           (for-each (lambda (var) (indent (+ 2 tabs)) (printf "~a" var)) locals))
         
         (unless (null? guards)
           (indent (+ 1 tabs)) (if (null? locals) (printf "when~n") (printf "where~n"))
           (for-each (lambda (g) (indent (+ 2 tabs)) (printf "~a~n" g)) guards))
           
         (unless (null? actions)
           (indent (+ 1 tabs)) (printf "then~n")
           (for-each (lambda (a) (indent (+ 2 tabs)) (printf "~a~n" a)) actions))
         
         (indent tabs) (printf "End~n"))))))

(define typed-event? 
  (match-lambda 
    ((struct Event (name status locals guards actions))
     (and (andmap typed-variable? locals)
          (andmap (compose typed-predicate? Labelled-struct) guards)
          (andmap (compose typed-action? Labelled-struct) actions)))))

(provide/contract
 [struct Event ((name symbol?)
                (status (one-of/c 'normal 'convergent))
                (locals (or/c (listof Variable?) (listof typed-variable?)))
                (guards (listof labelled-predicate?))
                (actions (listof labelled-action?)))]
 [typed-event? (Event? . -> . boolean?)])