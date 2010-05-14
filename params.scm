#lang scheme

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