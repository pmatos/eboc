#lang scheme

(require "value.scm"
         "number.scm"
         "shortcuts.scm")

(define (implicit-set a b)
  (new ImplicitSet% [min (number a)] [max (number b)]))

(define ImplicitSet%
  (class object%
    (init-field min)
    (init-field max)
    
    (super-new)
    
    (define/public (contains x)
      (and (eb<= min x) (eb<= x max)))))
           
(provide implicit-set
         ImplicitSet%)