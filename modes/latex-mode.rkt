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

;; Latex mode
(require "latex-mode/ast2latex.rkt"
         "../ast.rkt"
         "../params.rkt"
         "../macro-utils.rkt")

(provide latex-mode)

;; args: (listof (cons symbol string))
(define (latex-mode mch args)
  ;(check-args/latex! args)
  
  (time-apply-if "Latex Conversion" (verbose-mode)
                 (lambda ()
                   (let ([latex-filename (string-append (Machine-name mch) ".tex")])
                     (call-with-output-file latex-filename
                       (lambda (fp) (machine->latex mch fp))
                       #:mode 'text
                       #:exists 'replace)
                     
                     (printf "Latex file output to: ~a~n" latex-filename)))))
    ;(when (verbose-mode) 
    ;  (print-instructions latex-filename))))