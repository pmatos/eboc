#lang scheme

(require (planet "ast/predexpr.scm" ("pjmatos" "eboc.plt" 1 0)))

(define (make-state) '())

(define (state? u)
  (and (list? u)
       (andmap (lambda (p)
                 (and (pair? p)
                      (Variable? (car p))
                      (expr/wot? (cdr p))))
               u)))

(define (state-ref state var)
  (let ([binding (assf (lambda (el) (variable=? el var)) state)])
    (if binding
        (cdr binding)
        #f)))

(define (state-update state var value)
  (let ([rem-var (filter (lambda (p)
                           (not (variable=? (car p) var)))
                         state)])
    (cons (cons var value) rem-var)))

(define state-set state-update)

(provide/contract
 [make-state (-> state?)]
 [state? (any/c . -> . boolean?)]
 [state-ref (state? Variable? . -> . (or/c false/c expr/wot?))]
 [state-set (state? Variable? expr/wot? . -> . state?)]
 [state-update (state? Variable? expr/wot? . -> . state?)])