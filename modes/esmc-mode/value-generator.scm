#lang scheme

(require "enumerations.scm"
         "probabilities.scm")

;; This module builds on value enumerations to enumerate values of any eventb type in sequence.
;; For example: POW(Z), POW(B * Z), a, POW(a * Z) , etc where a is an enumerated set.
(define (type-enumeration t)
  (match t
    ('INT int-enumeration)
    ('BOOL (gen-finite-enumeration '(true false)))
    ((list (? symbol?) ': els) (gen-finite-enumeration els))
    (`(,subtype1 x ,subtype2)
     (let ([tenum1 (type-enumeration subtype1)]
           [tenum2 (type-enumeration subtype2)]
           [penum (gen-pair-enumeration (type-cardinality subtype1) (type-cardinality subtype2))])
       (lambda (n)
         (match-let ([(cons n1 n2) (penum n)])
           (cons (tenum1 n1) (tenum2 n2))))))
    (`(POW ,subtype)
     (let ([tenum (type-enumeration subtype)])
       (lambda (n)
         (map tenum (set-enumeration n)))))))

(define (type-cardinality t)
  (match t
    ('INT +inf.0)
    ('BOOL 2)
    ((list (? symbol?) ': els) (length els))
    (`(,subtype1 x ,subtype2)
     (* (type-cardinality subtype1) (type-cardinality subtype2)))
    (`(POW ,subtype)
     (expt 2 (type-cardinality subtype)))))

(define (type-enumeration/prt t)
  (match t
    ('BOOL (lambda (n) 1.0))
    ((list (? symbol?) ': els) (lambda (n) 1.0))
    (else prob)))

(define (type-list-enumerator types)
  (let* ([type-enumerations (map type-enumeration types)]
         [type-enumerations/prt (map type-enumeration/prt types)]
         [enum (natural-list-enumerator (map type-cardinality types))]
         [next-enum (enum)])
    (case-lambda [() 
                  (if (not next-enum)
                      #f
                      (let ([next-enum-lst (map (lambda (t-enum val) (t-enum val)) type-enumerations next-enum)])
                        (begin0 
                          next-enum-lst
                          (set! next-enum (enum)))))]
                 [(msg)
                  (case msg
                    [(prt) 
                     (if next-enum
                         (foldl * 1.0 (map (lambda (prob-enum val) (prob-enum val)) type-enumerations/prt next-enum))
                         0)]
                    [else (error 'type-list-enumerator "Can't understand message : ~a" msg)])])))

(provide type-list-enumerator)