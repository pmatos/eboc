#lang scheme

(require "value.scm")
(require "implicitset.scm")


(define (number n)
  (new Number% [num n]))

;; This implements the class Number
(define Number%
  (class* Value%
    (iNumber)
    (super-new)
    
    (init-field num)
 
    (define (apply-to-number op b)
      (op num (send b num)))
    
    ;; Relational Operators
    (define/public (equals b)
      (apply-to-number = b))
    
    (define/public (geq b)
      (apply-to-number <= b))
    
    (define/public (leq b)
      (apply-to-number >= b))
      
    (define/public (lt b)
      (apply-to-number < b))
    
    (define/public (gt b)
      (apply-to-number > b))
                   
    ;; Arithmetic Operators
    (define/public (plus b)
      ((compose number apply-to-number) + b))
    
    (define/public (mult b)
      ((compose number apply-to-number) * b))
    
    (define/public (div b)
      ((compose number apply-to-number) quotient b))
    
    (define/public (minus b)
      ((compose number apply-to-number) - b))
    
    (define/public (mod b)
      ((compose number apply-to-number) modulo b))
    
    ;; Set Operators
    (define/public (upto b)
      (implicit-set num (send b num)))))
    
(provide number
         Number%)