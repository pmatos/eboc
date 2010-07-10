#lang racket

(provide eboc-unreachable
         eboc-not-implemented)

(define (eboc-unreachable msg)
  (error "Unreachable code:" msg))

(define (eboc-not-implemented msg)
  (error "Feature not implemented:" msg))
