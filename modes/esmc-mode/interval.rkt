#lang scheme

(require "../../errors.scm")

(define make-int cons)
(define low car)
(define high cdr)

(define (integer-or-inf? u)
  (or (integer? u) 
       (= u +inf.0)
       (= u -inf.0)))

(define (inexact->exact/inf x)
  (if (or (= x +inf.0) (= x -inf.0))
      x
      (inexact->exact x)))

(define (interval? u)
  (and (pair? u) 
       (integer-or-inf? (low u))
       (integer-or-inf? (high u))))

(define (int-in x i)
  (<= (car i) x (cdr i)))

(define (int= i j)
  (and (= (low i) (low j))
       (= (high i) (high j))))

(define (int-minus i j)
  ;; Possible cases:
  ;; - i and j intersect on the low end of i (low j is not in i but high j is), result is the high end of i
  ;; - i and j intersect of the high end of i (high j is not in i but low j is). result is the low end of i
  ;; - j is contained strictly in i (low and high j are in i and are different from low i and low j), result is a split in i
  ;; - i and j are the same, so empty interval is returned #f
  ;; - i and j do not intersect, result is i
  (let ([low-i (low i)]
        [high-i (high i)]
        [low-j (low j)]
        [high-j (high j)])
    (cond
      ;;   i:    |-------|
      ;;   j: |+++++++++++|
      [(<= low-j low-i)
       (cond 
         ;; i:     |--------|
         ;; j: |++|
         [(< high-j low-i)
          i]
         ;; i:     |--------|
         ;; j: |---++++++++|
         [(< high-j high-i)
          (list (cons (+ high-j 1) high-i))]
         ;; i:     |--------|
         ;; j: |------------++++|
         [else
          '()])]
      ;;   i:      |--------|
      ;;   j:       |++++++++++|
      [(<= low-j high-i)
       (cond
         ;; i:     |--------|
         ;; j:      |++++++|
         [(< high-j high-i)
          (list (cons low-i (- low-j 1))
                (cons (+ high-j 1) high-i))]
         ;; i:     |--------|
         ;; j:      |-------++++|
         [(<= high-i high-j)
          (list (cons low-i (- low-j 1)))]
         [else
          (eboc-unreachable)])]
      [(< high-i low-j)
       (list i)]
      [else
       (eboc-unreachable)])))
         
;; Intersection of intervals
(define (int-inter i j)
  (let ([low-i (low i)]
        [high-i (high i)]
        [low-j (low j)]
        [high-j (high j)])
    (let ([lowest (inexact->exact/inf (max low-i low-j))]
          [highest (inexact->exact/inf (min high-i high-j))])
      (if (> lowest highest)
          '()
          (list (make-int lowest highest))))))

(provide/contract 
 [integer-or-inf? (-> any/c boolean?)]
 [interval? (-> any/c boolean?)]
 [make-int (-> integer-or-inf? integer-or-inf? interval?)]
 [low (-> interval? integer-or-inf?)]
 [high (-> interval? integer-or-inf?)]
 [int= (-> interval? interval? boolean?)]
 [int-in (-> integer-or-inf? interval? boolean?)]
 [int-minus (-> interval? interval? (listof interval?))]
 [int-inter (-> interval? interval? (listof interval?))])