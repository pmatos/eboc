#lang scheme

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