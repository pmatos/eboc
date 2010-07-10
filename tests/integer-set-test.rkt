#lang racket

;; types tests
(require (planet schematics/schemeunit:3:4/text-ui)
         (planet schematics/schemeunit:3:4/test)
         (only-in srfi/1 every)
         "../modes/esmc-mode/integer-set.rkt")

(run-tests
 (test-suite
  "integer sets"
  
  (test-suite
   "equality"
   
   (test-case "simple1"
              (check iset= (iset-upto+ 10 20) (iset-upto+ 10 20)))
   
   (test-case "simple2"
              (check (compose not iset=) (iset-upto+ 10 20) (iset-upto+ 11 20)))
   
   (test-case "inf"
              (check iset= (make-integer-iset) (make-integer-iset))))
  
  (test-suite
   "union"
   
   (test-case "case1"
              (check iset= 
                     (iset-union (iset-upto+ 0 10) (make-integer-iset))
                     (make-integer-iset)))
   
   (test-case "case2"
              (check iset= 
                     (iset-union (iset-upto+ -10 10) (iset-upto+ 0 100))
                     (iset-upto+ -10 100)))
   
   (test-case "inf"
              (check iset=
                     (iset-union (make-natural-iset) (iset-upto+ -inf.0 0))
                     (make-integer-iset))))
  
  (test-suite
   "inter"
   
   (test-case "case1"
              (check iset= 
                     (iset-inter (make-integer-iset) (make-natural-iset))
                     (make-natural-iset)))
   
   (test-case "case2"
              (check iset=
                     (iset-inter (iset-upto+ 10 100) (iset-upto+ 15 334))
                     (iset-upto+ 15 100)))
   
   (test-case "case3"
              (check iset=
                     (iset-inter (iset-upto+ 10 100) (iset-upto+ 50 51))
                     (iset-upto+ 50 51))))
  
  (test-suite
   "minus"
   
   (test-case "case1"
              (check iset= 
                     (iset-setminus (make-integer-iset) (make-natural-iset))
                     (iset-upto+ -inf.0 -1)))
   
   (test-case "case2"
              (check iset=
                     (iset-setminus (make-integer-iset) (iset-upto+ 0 10))
                     (iset-union (iset-upto+ -inf.0 -1) (iset-upto+ 11 +inf.0)))))))