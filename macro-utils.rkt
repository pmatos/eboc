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

(provide time-apply-if
         dump-if)

;; Macro providing a facility to turn on/off time-apply.
;; Returns the result of applying proc to the list lst of arguments.
;; If test is true, it prints timing information using str as identifier for the timer.
;; otherwise prints nothing.
(define-syntax-rule (time-apply-if str test proc arg ...)
  (if test
      (let-values ([(val cpu real gc) (time-apply proc (list arg ...))])
        (printf "Timing for: ~a (cpu ~a, real ~a, gc ~a)~n" str (- cpu gc) real gc)
        (apply values val))
      (proc arg ...)))
      
(define-syntax-rule (dump-if filename test proc val)
  (when test
    (call-with-output-file filename
      (lambda (port) (proc val port))
      #:text 
      #:replace)))