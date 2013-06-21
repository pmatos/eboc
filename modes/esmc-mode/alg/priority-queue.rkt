#lang racket
;;
;;    This file is part of Eboc (Event-B Model Checker).
;;
;;    Eboc is free software: you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation, either version 3 of the License, or
;;    (at your option) any later version.
;;
;;    Eboc is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.
;;
;;    You should have received a copy of the GNU General Public License
;;    along with Eboc.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; priority-queue.ss  --  Jens Axel Søgaard
;;; PURPOSE

; This file implements priority queues on top of
; a heap library.

(require (prefix-in heap: "leftist-heap.rkt")
         srfi/67)

; a priority-queue is a heap of  (cons <priority> <element>)

(define-struct priority-queue (heap))

;;; CONTRACT PREDICATES

(define compare/c  (flat-named-contract 'compare-function procedure?))
(define pq/c       (flat-named-contract 'priority-queue priority-queue?))
(define list/c     (flat-named-contract 'list (lambda (o) (or (null? o) (pair? o)))))
(define boolean/c  (flat-named-contract 'boolean boolean?))
(define number/c   (flat-named-contract 'number number?))

; srfi-42
;(provide :heap heap-ec)
; values
(provide/contract 
 (delete-min          (-> pq/c  pq/c))
 (elements            (-> pq/c   list/c))
 (elements+priorities (->* (pq/c) () (values list/c list/c)))
 (empty               (case-> (-> pq/c) 
                              (-> compare/c pq/c)))
 (empty?              (-> pq/c   boolean/c))
 (find-min            (-> pq/c   any/c))
 (find-min-priority   (-> pq/c   number/c))
 (fold                (-> (-> any/c any/c  any/c) any/c pq/c    any/c))
 (insert              (-> any/c number/c pq/c    pq/c))
 (insert*             (-> list/c list/c pq/c   pq/c))
 (priority-queue?     (-> any/c boolean/c))
 (size                (-> pq/c  natural-number/c))
 (union               (-> pq/c pq/c   pq/c)))

; conveniences
(define (heap pq) (priority-queue-heap pq))
(define (pri p)   (car p))
(define (elm p)   (cdr p))
(define (make h)  (make-priority-queue h))

; sort after priority
; TODO: and then element?
(define (compare p1 p2)
  (number-compare (pri p1) (pri p2)))

;;; OPERATIONS

(define (elements pq)
  (map elm (heap:elements (heap pq))))

(define (elements+priorities pq)
  (let ([eps (heap:elements (heap pq))])
    (values (map elm eps)
            (map pri eps))))

(define (empty? pq)
  (heap:empty? (heap pq)))

(define empty
  (case-lambda 
    [()    (empty (current-compare))]
    [(cmp) (make (heap:empty compare))]))

(define (fold f b a)
  (heap:fold f b (heap a)))

(define (find-min pq) 
  (elm (heap:find-min (heap pq))))

(define (find-min-priority pq)
  (pri (heap:find-min (heap pq))))

(define (insert x p pq)
  (make (heap:insert (cons p x) (heap pq))))

(define (insert* xs ps pq)
  (make (heap:insert* (map cons ps xs) (heap pq))))

(define (delete-min pq)
  (make (heap:delete-min (heap pq))))

(define (size pq)
  (heap:size (heap pq)))

(define (union pq1 pq2)
  (make (heap:union (heap pq1) (heap pq2))))
