#lang scheme

(require (planet williams/science/random-distributions))

(provide prob)

(define (prob n)
  (let ([prob-0 (gaussian-pdf 0 0.0 1000000.0)])
    (* (gaussian-pdf n 0.0 1000000.0) (/ 1.0 prob-0))))