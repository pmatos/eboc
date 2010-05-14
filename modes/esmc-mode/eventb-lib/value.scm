#lang scheme

;; This module implements the main Event-B Class.
;; The Value is a class from which all other Event-B value classes
;; must inherit.

(define iValue
  (interface () in equals notequals))

(define iNumber 
  (interface (iValue)
    geq leq lt gt
    plus minus mult div mod
    upto))

(define iSet
  (interface (iValue)
    contains))

(define Value%
  (class object%
    (super-new)
    
    (define/public (in x)
      (send x contains this))
    
    (define/public (notequals x)
      (not (send this equals x)))))

(provide Value%
         iValue
         iNumber
         iSet
;         iPair
         )