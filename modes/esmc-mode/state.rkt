#lang scheme

(require (planet "ast/predexpr.scm" ("pjmatos" "eboc.plt" 1 0)))

(define (make-state) '())

(define (state-lhs? u)
  (or (Variable? u)
      (Constant? u)
      (Set? u)))

;; Comparison function for state lhs
(define (state-lhs= u1 u2)
  (cond [(and (Constant? u1) (Constant? u2))
         (constant=? u1 u2)]
        [(and (Variable? u1) (Variable? u2))
         (variable=? u1 u2)]
        [(and (Set? u1) (Set? u2))
         (set=? u1 u2)]
        [else #f]))

(define (state-length s)
  (length s))

(define (state= s1 s2)
  (and (= (state-length s1) (state-length s2))
       (andmap (lambda (s1-binding)
                 (let* ([s1-lhs (car s1-binding)]
                        [s1-rhs (cdr s1-binding)]
                        [s2-rhs (state-ref s2 s1-lhs)])
                   (if s2-rhs
                       (expression/wot= s1-rhs s2-rhs)
                       #f)))
               s1)))

(define (state? u)
  (and (list? u)
       (andmap (lambda (p)
                 (and (pair? p)
                      (state-lhs? (car p))
                      (expr/wot? (cdr p))))
               u)))

(define (state-ref state var)
  (let ([binding (assf (lambda (el) (state-lhs= el var)) state)])
    (if binding
        (cdr binding)
        #f)))

(define (state-update state var value)
  (let ([rem-var (filter (lambda (p)
                           (not (state-lhs= (car p) var)))
                         state)])
    (cons (cons var value) rem-var)))

(define state-set state-update)

(define (state-merge s1 . states)
  (append s1 (apply append states)))

(define (state-print state (out (current-output-port)))
    (parameterize ([current-output-port out])
      (for-each (match-lambda ((cons var val) (printf "~a : ~a" var val)))
                state)))

(provide/contract
 [make-state (-> state?)]
 [state? (any/c . -> . boolean?)]
 [state-ref (state? state-lhs? . -> . (or/c false/c expr/wot?))]
 [state-set (state? state-lhs? expr/wot? . -> . state?)]
 [state-merge ((state?) () #:rest (listof state?) . ->* . state?)]
 [state-update (state? state-lhs? expr/wot? . -> . state?)]
 [state-print ((state?) (output-port?) . ->* . void?)]
 [state= (state? state? . -> . boolean?)])