#lang racket

(require (planet schematics/schemeunit:3:4/text-ui)
         (planet schematics/schemeunit:3:4/test)
         (only-in srfi/1 every)
         "../ast/predexpr.rkt")

(require/export "../pass-typing.rkt"
                (pass-predicate))

(run-tests
 (test-suite
  "pass-typing"
  
  (test-suite
   "typing-algorithm"
   
   (test-begin
    (let ([pred (p `((x in Z) land (1 le x)))])
      (let-values ([(vars env eqs typed-pred)
                    (pass-predicate pred '() (make-empty-env Variable? variable=?) '())])
        (check predicate=?
               (make-Predicate-BinOp)
               (retype-predicate typed-pred vars env eqs)))))
   
   (test-begin
    (let ([pred (p `(emptyset equal emptyset))])
      (let-values ([(vars env eqs typed-pred)
                    (pass-predicate pred '() (make-empty-env Variable? variable=?) '())])
        (check-exn exn?
                   (lambda ()
                     (retype-predicate typed-pred vars env eqs)))))))))
                        