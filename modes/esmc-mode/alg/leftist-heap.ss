#lang scheme
(require srfi/67
         srfi/42)

;;; leftist-heap.scm  --  Jens Axel Søgaard  

;;; HISTORY
; 28th dec 2005
;   Initial version
; 3rd june 2009 
;   Brushup for 4xx

;;; LEFTIST HEAP [Okasaki, p.17-20]

; A *Leftist heap* is a heap-ordered binary tree with the /leftist property/:
; The rank of any left child is at least as large as the rank of its right sibling.
; The *rank* of a node is the length of the its *right spine*, which is the
; rightmost path from the node to an empty node.

;;; Time             worst case
;  delete-min         O(log n)
;  empty              O(1)
;  empty?             O(1)
;  find-min           O(1)
;  insert             O(log n)
;  union              O(log n)


;;; DATA DEFINITION

; A HEAP is either
;    (make-heap-empty cmp)
; or
;    (make-heap-node cmp rank elm left right)
; where
;   cmp  is a compare function,
;   rank is an integer, and
;   left and right are heaps.

(define-struct heap              (compare))
(define-struct (heap-empty heap) ()
  #:property prop:sequence 
  (λ (heap)
    (make-do-sequence
     (λ () (values
            (λ (h) #f) (λ (h) #f) heap
            (λ (h) #f) (λ (h) #f) (λ (h p) #f))))))
        
(define-struct (heap-node heap)  (rank elm left right)
  #:property prop:sequence 
  (λ (heap)
    (make-do-sequence
     (λ ()
       (values
        ; the position is simply the heap
        (λ (h) (find-min h))
        (λ (h) (delete-min h))
        heap
        (λ (h) (not (empty? h)))
        (λ (h) (not (empty? h)))
        (λ (h p) (not (empty? h))))))))
  

;;; CONTRACT PREDICATES

(define compare/c  (flat-named-contract 'compare-function procedure?))
(define heap/c     (flat-named-contract 'heap heap?))
(define list/c     (flat-named-contract 'list (lambda (o) (or (null? o) (pair? o)))))
(define boolean/c  (flat-named-contract 'boolean boolean?))
             
;;; EXPORTED OPERATIONS

(provide empty? find-min delete-min)

(provide/contract 
 (count      (-> any/c heap/c   natural-number/c)) 
 (delete     (-> any/c heap/c   heap/c))
 (delete-all (-> any/c heap/c   heap/c))
 #;(delete-min (-> heap/c   heap/c))
 (elements   (-> heap/c   list/c))
 (empty      (case-> (->             heap/c) 
                     (-> compare/c   heap/c)))
 #; (empty?     (-> heap/c   boolean/c))
 #;(find-min   (-> heap/c   any/c))
 (fold       (-> (-> any/c any/c  any/c) any/c heap/c    any/c))
 (get        (-> any/c heap/c any/c))
 (rename -heap heap   ; export -heap as heap
             (->* () () #:rest any/c heap/c))
 (heap?      (-> any/c boolean/c))
 (insert     (-> any/c heap/c    heap/c))
 (insert*    (-> list/c heap/c   heap/c))
 (list->heap (case-> (-> list/c            heap/c) 
                     (-> compare/c list/c  heap/c)))
 (singleton  (case-> (-> any/c            heap/c) 
                     (-> compare/c any/c  heap/c)))
 (select     (-> heap/c  any/c))
 (size       (-> heap/c  natural-number/c))
 (union      (-> heap/c heap/c   heap/c)))

(provide :heap heap-ec)  ; srfi/42
(provide in-heap)        ; sequences

;;; CORE HEAP OPERATIONS

(define empty
  (case-lambda 
    [()    (make-heap-empty (current-compare))]
    [(cmp) (make-heap-empty cmp)]))

(define empty? heap-empty?)

(define (rank h)
  (if (empty? h)
      0
      (heap-node-rank h)))

(define (make x a b)
  (let ([ra (rank a)] 
        [rb (rank b)]
        [cmp (heap-compare a)])
    (if (>= ra rb)
        (make-heap-node cmp (add1 rb) x a b)
        (make-heap-node cmp (add1 ra) x b a))))

(define (union h1 h2)
  (cond
    [(empty? h1) h2]
    [(empty? h2) h1]
    [else        (let ([x (heap-node-elm h1)]
                       [y (heap-node-elm h2)])
                   (if<=? ((heap-compare h1) x y)
                          (make x (heap-node-left h1) (union (heap-node-right h1) h2))
                          (make y (heap-node-left h2) (union h1 (heap-node-right h2)))))]))

(define (insert x h)
  (let ([cmp (heap-compare h)])
    (union (make-heap-node cmp 1 x (make-heap-empty cmp) (make-heap-empty cmp))
           h)))

(define (delete x h)
  (define (delete/sf x h s f)
    (cond
      [(empty? h)                    
       (s h)]
      [(=? (heap-compare h) x (heap-node-elm h))
       (s (union (heap-node-left h) (heap-node-right h)))]
      [(<? (heap-compare h) x (heap-node-elm h))
       (f)]
      [else                          
       (let ([cmp (heap-compare h)])
         (let ([y (heap-node-elm h)]
               [l (heap-node-left h)]
               [r (heap-node-right h)])
           (delete/sf x l
                      (lambda (h1) (s (make y h1 r)))
                      (lambda ()   (delete/sf x r
                                              (lambda (h1) (s (make y l h1)))
                                              (lambda () (f)))))))]))
  (delete/sf x h 
             (lambda (h1) h1) 
             (lambda ()   h)))

(define (delete-all x h)
  (define (delete-all/sf x h s f)
    (cond
      [(empty? h)                    
       (s h)]
      [(=? (heap-compare h) x (heap-node-elm h))
       (s (union (delete-all x (heap-node-left h)) 
                 (delete-all x (heap-node-right h))))]
      [(<? (heap-compare h) x (heap-node-elm h))
       (f)]
      [else                          
       (let ([cmp (heap-compare h)])
         (let ([y (heap-node-elm h)]
               [l (heap-node-left h)]
               [r (heap-node-right h)])
           (delete-all/sf x l
                          (lambda (l1) (s (delete-all/sf x r
                                                         (lambda (r1) (make y l1 r1))
                                                         (lambda ()   (make y l1 r)))))
                          (lambda ()   (delete-all/sf x r
                                                      (lambda (r1) (s (make y l r1)))
                                                      (lambda ()   (f)))))))]))
  (delete-all/sf x h 
                 (lambda (h1) h1) 
                 (lambda ()   h)))

(define (find-min h)
  (heap-node-elm h))

(define (delete-min h)
  (union (heap-node-left h) (heap-node-right h)))

(define (get x h)
  (let ([cmp (heap-compare h)])
    (define (inner-get h s f)
      (if (empty? h)
          (f)
          (if=? (cmp x (heap-node-elm h))
                (s (heap-node-elm h))
                (inner-get (heap-node-left h) s
                           (lambda () (inner-get (heap-node-right h) s 
                                                 f))))))
    (inner-get h (lambda (x) x) (lambda () #f))))

;;;
;;; EXTRA OPERATIONS
;;;

(define (delete* xs h)
  (foldl delete h xs))

(define (elements h)
  (fold cons '() h))

(define (fold f b h)
  (if (empty? h)
      b
      (fold f 
            (fold f 
                  (f (heap-node-elm h) b)
                  (heap-node-left h))
            (heap-node-right h))))

(define (count x h)
  (let ([cmp (heap-compare h)])
    (fold (lambda (y s)
            (if=? (cmp x y)
                  (add1 s)
                  s))
          0 h)))

(define (-heap . xs)
  (list->heap xs))

(define (insert* xs h)
  (union (list->heap (heap-compare h) xs) h))

(define list->heap
  ; time: O(n)
  (case-lambda 
    [(l)     (list->heap (current-compare) l)]
    [(cmp l) (let* ([e  (empty cmp)]
                    [hs (map (lambda (x) (insert x e)) l)])
               ; (list heap) -> (list heap)
               ;  merge adjacent pairs of heaps
               (define (merge-pairs hs)
                 (cond
                   [(or (null? hs)
                        (null? (cdr hs))) hs]
                   [else (cons (union (car hs) (cadr hs))
                               (merge-pairs (cddr hs)))]))
               (if (null? hs)
                   e
                   (let loop ([hs hs])
                     ; merge adjacent pairs of heaps until one is left
                     (cond
                       [(null? hs)       '()]
                       [(null? (cdr hs)) (car hs)]
                       [else             (loop (merge-pairs hs))]))))]))

; select : set -> element
(define (select s)
  (if (empty? s)
      (error 'select "can't select an element from an empty heap")
      (find-min s)))

(define singleton
  (case-lambda 
    [(x)     (insert x (empty))]
    [(cmp x) (insert x (make-heap-empty cmp))]))

(define (size h)
  ; NOTE: T(size)=O(n)
  (cond
    [(heap-empty? h) 0]
    [else            (+ (size (heap-node-left h))
                        1
                        (size (heap-node-right h)))]))

(define (in-heap heap)
  (make-do-sequence
   (λ ()
     (values
      ; the position is simply the heap
      (λ (h) (find-min h))
      (λ (h) (delete-min h))
      heap
      (λ (h) (not (empty? h)))
      (λ (h) (not (empty? h)))
      (λ (h p) (not (empty? h)))))))
  
(define-derived-comprehension heap-ec ()
  ((heap-ec cmp etc ... body)
   (etc ...)
   (list->heap cmp (list-ec  etc ... body))))

(define-generator (:heap stx)
  (syntax-case stx (index)
    ((:heap var (index i) arg)
     #'(:parallel  (:stack var arg) (:integers i)) )
    ((:heap var arg)
     #'(:do (let ())
            ((t arg))
            (not (empty? t))
            (let ((var (find-min t))))
            #t
            ((delete-min t)) ))))

(define (:heap-dispatch args)
  (cond
    [(null? args)
     'heap]
    [(and  (heap? (car args)))
     (:generator-proc (:heap (car args)))]
    [else
     #f]))

(:-dispatch-set! 
 (dispatch-union (:-dispatch-ref) :heap-dispatch))
  