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

;; Command Line Arguments Parsing
;(: verbose-mode (Parameter Boolean))
(define verbose-mode (make-parameter #t))

(define no-header? (make-parameter #f))

  ;(: mode-option (Parameter Assoc))
(define mode-options (make-parameter '()))

;(: model-checking-mode (Parameter Symbol))
; Valid values : 
; - escm: Explicit State Model Checking
; - composition: Composition of Models
; - latex: LaTeX output generation
(define execution-mode (make-parameter 'esmc))

(define working-directory (make-parameter (current-directory)))

;; Printing parameters
(define spaces/tab (make-parameter 2))
(define print-debug? (make-parameter #f))

(provide/contract
 [verbose-mode (parameter/c boolean?)]
 [no-header? (parameter/c boolean?)]
 [mode-options (parameter/c (listof (cons/c symbol? string?)))]
 [execution-mode (parameter/c symbol?)]
 [working-directory (parameter/c path?)]
 [spaces/tab (parameter/c (and/c integer? positive?))]
 [print-debug? (parameter/c boolean?)])