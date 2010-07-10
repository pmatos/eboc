#lang racket

(provide listof?
         dict-merge)

(require (only-in srfi/1 every))

;; Returns true if lst is a list of elements accepted by proc.
;; So if proc is a predicate for integers, listof? tests if
;; lst is a list of integers.
(define (listof? proc u)
  (and (list? u)
       (every proc u)))

;; Merges several dicts assuming that the sets of keys are disjoint
(define dict-merge 
  (case-lambda [(dict) dict]
               [(dict1 dict2)
                (let loop ([res dict1] [next (dict-iterate-first dict2)])
                  (if next
                      (loop (dict-set res (dict-iterate-key dict2 next) (dict-iterate-value dict2 next))
                            (dict-iterate-next dict2 next))
                      res))]
               [(dict . dicts)
                (foldl (lambda (el acum) (dict-merge acum el))
                       dict
                       dicts)]))