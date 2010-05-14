#lang scheme

(define (eb<= a b)
  (send a leq b))

(define (eb+ a b)
  (send a plus b))
  
(define (eb- a b)
  (send a minus b))

(define (eb* a b)
  (send a mult b))

(define (eb/ a b)
  (send a div b))

(define (ebmod a b)
  (send a mod b))

(provide (all-defined-out))