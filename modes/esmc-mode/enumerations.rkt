#lang racket

;; This module implements enumerations over values of the following types:
;; val? is one of:
;; - int?
;; - (cons val? val?)
;; - (set val?) ;; homogeneous non-fixed sized sets

;; This one is not considered a value but will also subject of enumeration
;; - (list n val? ;; homogeneous fixed n-size lists 

(require (planet "evector.scm" ("soegaard" "evector.plt" 1 1))
         (only-in srfi/1 xcons)
         srfi/43)


;                                                          
;                                                          
;                                                          
;   ;   ;          ;;                         ;;;          
;   ;;  ;          ;;                           ;          
;   ;;  ;   ;;;   ;;;;;  ;   ;   ; ;;;  ;;;     ;     ;;;  
;   ; ; ;      ;   ;;    ;   ;   ;;        ;    ;    ;;    
;   ; ; ;      ;   ;;    ;   ;   ;         ;    ;    ;;    
;   ; ; ;   ;;;;   ;;    ;   ;   ;      ;;;;    ;     ;;;  
;   ;  ;;  ;   ;    ;    ;   ;   ;     ;   ;    ;        ; 
;   ;  ;;  ;   ;    ;    ;   ;   ;     ;   ;    ;        ; 
;   ;   ;  ;;;;;    ;;;   ;; ;   ;     ;;;;;    ;;;  ;;;;  
;                                                          
;                                                          
;                                                          
;; Trivial
(define (nat-enumeration n) n)

(define (nat-enumerator)
  (let ([n 0])
    (lambda ()
      (begin0 
        n
        (set! n (+ n 1))))))

;                                                          
;                                                          
;                                                          
;   ;;;;;          ;;                                      
;     ;            ;;                                      
;     ;    ;;;;   ;;;;;   ;;;    ;;;;   ;;;    ; ;;;  ;;;  
;     ;    ;;  ;   ;;    ;;  ;  ;  ;;  ;;  ;   ;;    ;;    
;     ;    ;   ;   ;;    ;   ;  ;   ;  ;   ;   ;     ;;    
;     ;    ;   ;   ;;    ;;;;;  ;   ;  ;;;;;   ;      ;;;  
;     ;    ;   ;    ;    ;      ;   ;  ;       ;         ; 
;     ;    ;   ;    ;    ;      ;  ;;  ;       ;         ; 
;   ;;;;;  ;   ;    ;;;   ;;;;   ;;;;   ;;;;   ;     ;;;;  
;                                   ;                      
;                                   ;                      
;                                ;;;                       

;; REF: Wikipedia on enumerations
;; http://en.wikipedia.org/wiki/Enumerative

(define (int-enumeration n)
  (if (odd? n)
      (- (/ (+ n 1) 2))
      (/ n 2)))

(define (int-enumerator)
  (let ([n 0])
    (lambda ()
      (begin0
        (int-enumerator n)
        (set! n (+ n 1))))))

;                                     
;                                     
;                                     
;   ;;;;;           ;                 
;   ;   ;                             
;   ;   ;;  ;;;    ;;     ; ;;;  ;;;  
;   ;   ;      ;    ;     ;;    ;;    
;   ;;;;;      ;    ;     ;     ;;    
;   ;       ;;;;    ;     ;      ;;;  
;   ;      ;   ;    ;     ;         ; 
;   ;      ;   ;    ;     ;         ; 
;   ;      ;;;;;  ;;;;;   ;     ;;;;  
;                                     
;                                     
;                                     
;; Theres a functional pearl on enumeration rationals which explains on how
;; to enumerate pairs in the first few paragraphs.

(define (pair-enumeration/infinite n)
  (let* ([sum (inexact->exact (floor (/ (- (sqrt (+ (* 8 n) 2)) 1) 2)))]
         [sum-start (/ (* sum (+ sum 1)) 2)]
         [index (- n sum-start)])
    (cons (- sum index) index)))

(define (gen-pair-enumeration/finite s1 s2)
  (lambda (n)
    (if (n . >= . (* s1 s2))
        #f
        (call-with-values (lambda () (quotient/remainder n s2))
                          cons))))
     
(define (gen-pair-enumeration/mixed s first?)
  (lambda (n)
    (call-with-values (lambda () (quotient/remainder n s)) 
                      (if first? cons xcons))))

(define (gen-pair-enumeration s1 s2)
  (cond [(and (eq? s1 +inf.0) (eq? s2 +inf.0)) pair-enumeration/infinite]
        [(eq? s1 +inf.0) (gen-pair-enumeration/mixed s2 #t)]
        [(eq? s2 +inf.0) (gen-pair-enumeration/mixed s1 #f)]
        [else (gen-pair-enumeration/finite s1 s2)]))

;(define (pair-enumerator)
;  (let ([n 0])
;    (lambda ()
;      (begin0
;        (pair-enumeration n)
;        (set! n (+ n 1))))))
;; TODO: MISSING ENUMERATIONS FOR FINITE AND MIXED VERSIONS OF PAIR-ENUMERATION

;                                            
;                                            
;                                            
;   ;;;;;    ;             ;     ;;          
;   ;;                           ;;          
;   ;;      ;;    ;;;;    ;;    ;;;;;   ;;;  
;   ;;       ;    ;;  ;    ;     ;;    ;;  ; 
;   ;;;;;    ;    ;   ;    ;     ;;    ;   ; 
;   ;;       ;    ;   ;    ;     ;;    ;;;;; 
;   ;;       ;    ;   ;    ;      ;    ;     
;   ;;       ;    ;   ;    ;      ;    ;     
;   ;;     ;;;;;  ;   ;  ;;;;;    ;;;   ;;;; 
;                                            
;                                            
;                                            
;; Returns an enumerator for a finite list/set
(define (gen-finite-enumerator lst)
  (lambda ()
    (if (null? lst) 
        #f
        (begin0 
          (car lst)
          (set! lst (cdr lst))))))

(define (gen-finite-enumeration lst)
  (let* ([vec (list->vector lst)]
         [sz (vector-length vec)])
    (lambda (n)
      (if (n . >= . sz)
          #f
          (vector-ref vec n)))))
    

;                              
;                              
;                              
;    ;;;;          ;;          
;   ;              ;;          
;   ;       ;;;   ;;;;;   ;;;  
;   ;;     ;;  ;   ;;    ;;    
;    ;;;   ;   ;   ;;    ;;    
;       ;  ;;;;;   ;;     ;;;  
;       ;  ;        ;        ; 
;       ;  ;        ;        ; 
;   ;;;;    ;;;;    ;;;  ;;;;  
;                              
;                              
;       
;; Enumeration of sets [tricky stuff I tell you!]
;; We will use a fairness step...
;; We generate the empty set, then we generate a set of size 1, step times, at which time we build a set of size 2.
;; basically for each step sets of size n, we build one of size (+ n 1)
(define (set-enumerator #:fairstep (step 2))
  (let ([maxsize 1]
        [count-vec (make-evector 1 0)]
        [enum-vec (make-evector 1 (natural-set-enumerator 0))])
    (lambda ()
      (let loop ([sz 0])
        (cond [(sz . = . maxsize) 
               (evector-set! count-vec sz 0)
               (evector-set! enum-vec sz (natural-set-enumerator sz))
               (set! maxsize (+ maxsize 1))
               (loop sz)]
              [((evector-ref count-vec sz) . = . step)
               (evector-set! count-vec sz 0)
               (loop (add1 sz))]
              [else
               (let ([val ((evector-ref enum-vec sz))])
                 (if val
                     (begin
                       (evector-set! count-vec sz (add1 (evector-ref count-vec sz)))
                       val)
                     (loop (+ sz 1))))])))))

;; Generates an enumerator for sets of naturals of a given size >= 2, with maximum natural k, size >= k
(define (natural-set-enumerator sz (k +inf.0))
  (case sz 
    [(0) 
     (let ([done? #f])
       (lambda ()
         (if done? 
             #f
             (begin0 
               '()
               (set! done? #t)))))]
    [(1)
     (let ([n 0])
       (lambda ()
           (if (> n k)
               #f
               (begin0
                 (list (nat-enumeration n))
                 (set! n (+ n 1))))))]
    [else
     (let* ([major (- sz 1)]
            [tail-enum (natural-set-enumerator (- sz 1) (- major 1))])
       (lambda ()
         (let ([next-tail (tail-enum)])
           (if next-tail
               (cons major next-tail)
               (if (= major k) ;; cannot increase major anymore
                   #f
                   (let ([next-major (+ major 1)]
                         [next-tail-enum (natural-set-enumerator (- sz 1) major)])
                     (begin0
                       (cons next-major (next-tail-enum))
                       (set! major next-major)
                       (set! tail-enum next-tail-enum))))))))]))

(define set-enumeration
  (let* ([enum (set-enumerator)]
         [next-index 1]
         [vec (make-evector 1 (enum))])
    (lambda (n)
      (if (n . < . next-index)
          (evector-ref vec n)
          (begin
            (for-each (lambda (k) (evector-set! vec k (enum)))
                      (build-list (add1 (- n next-index)) (lambda (x) (+ x next-index))))
            (set! next-index (add1 n))
            (evector-ref vec n))))))

;                                     
;                                     
;                                     
;   ;        ;            ;;          
;   ;                     ;;          
;   ;       ;;     ;;;   ;;;;;   ;;;  
;   ;        ;    ;;      ;;    ;;    
;   ;        ;    ;;      ;;    ;;    
;   ;        ;     ;;;    ;;     ;;;  
;   ;        ;        ;    ;        ; 
;   ;        ;        ;    ;        ;  
;   ;;;;;; ;;;;;  ;;;;     ;;;  ;;;;  
;                                     
;                                     
;                                     
;; Given a list with the maximum size of each element, 
;; returns all the distinct lists of a given size summing sum
(define (natural-list-enumerator/all szs sum)
  (let ([len (length szs)])
    (cond [(and (len . = . 1) ((first szs) . <= . sum)) '()]
          [(len . = . 1) (list (list sum))]
          [else 
           (append-map (lambda (head)
                         (map (lambda (tail) (cons head tail)) 
                              (natural-list-enumerator/all (rest szs) (- sum head))))
                       (build-list (inexact->exact (min (first szs) (+ sum 1))) (lambda (x) x)))])))

(define (natural-list-enumerator szs)
  (let ([vals (natural-list-enumerator/all szs 0)]
        [next-sum 1]
        [finished? #f])
    (lambda ()
      (if finished?
          #f
          (if (not (null? vals))
              (begin0 
                (first vals)
                (set! vals (rest vals)))
              (let ([next-vals (natural-list-enumerator/all szs next-sum)])
                (if (null? next-vals)
                    (begin0
                      #f
                      (set! finished? #t))
                    (begin0
                      (first next-vals)
                      (set! vals (rest next-vals))
                      (set! next-sum (+ next-sum 1))))))))))


  

(provide 
 natural-list-enumerator
 set-enumerator
 set-enumeration
 gen-finite-enumerator
 gen-finite-enumeration
 ;pair-enumerator
 gen-pair-enumeration
 int-enumerator
 int-enumeration
 nat-enumerator
 nat-enumeration)