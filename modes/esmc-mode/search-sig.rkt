#lang racket

(define-signature search^
  ((contracted
    (num-states (-> integer?))
    (hash-states? (-> boolean?))
    (debug? (-> boolean?)))))

(provide search^)
