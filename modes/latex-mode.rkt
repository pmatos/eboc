#lang racket

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