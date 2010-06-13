#lang scheme

(require scheme/serialize)

(define (deserialize-assoc assl)
  (map (lambda (p)
         (cons (deserialize  (car p))
               (deserialize (cdr p))))
       assl))

(provide deserialize-assoc)