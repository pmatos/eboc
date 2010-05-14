#lang scheme

;; Simulation library
;; Integers are Scheme Integers
;; Pairs are Scheme Pairs
;; Sets are RB Trees based on sooegard data structures library
(require (only-in srfi/13 string-prefix?)
         (only-in srfi/1 car+cdr)
         "../../untyped-utils.scm"
         "../../errors.scm"
         mzlib/etc ;; for identity
         (prefix-in set: (planet soegaard/galore:4:1/set))
         "interval.scm")

                                         
;                                                          
;                                                          
;   ;;;;;          ;;            ;;;;          ;;          
;     ;            ;;           ;              ;;          
;     ;    ;;;;   ;;;;;         ;       ;;;   ;;;;;   ;;;  
;     ;    ;;  ;   ;;           ;;     ;;  ;   ;;    ;;    
;     ;    ;   ;   ;;            ;;;   ;   ;   ;;    ;;    
;     ;    ;   ;   ;;               ;  ;;;;;   ;;     ;;;  
;     ;    ;   ;    ;               ;  ;        ;        ; 
;     ;    ;   ;    ;               ;  ;        ;        ; 
;   ;;;;;  ;   ;    ;;;         ;;;;    ;;;;    ;;;  ;;;;  
;                                                          
;                                                          
;                                                          


;
;;; Infinite Integer Sets
;(define (make-empty-iset)
;  (lambda (op . args)
;    (case op
;      [(card) 0]
;      [(min) (error "Empty set has no minimum.")]
;      [(max (error "Empty set has no maximum."))]
;      [(in) #f]
;      [(union) (first args)]
;      [(inter) (make-empty-set)]
;      [(setminus) (make-empty-set)])))
;
;(define (make-integer-iset)
;  (lambda (op . args)
;    (case op
;      [(card) +inf.0]
;      [(min) -inf.0]
;      [(max) +inf.0]
;      [(in) #t]
;      [(union) (make-integer-iset)]
;      [(inter) (first args)]
;      [(setminus)
;       (let ([s (first args)])
;         (lambda (op . args)
;           (case op
;             [(card) 
;              ;; The problem is that we don't have a good way to determine the
;              ;; size of a set if we are subtracting infinite sets, for example:
;              ;; card(Z - {2*x | x : Z}) = +inf
;              ;; card(Z - N) = +inf
;              ;; card(Z - (Z - 0)) = 1
;              (eboc-not-implemented "cardinality of (Z - x), for a set x.")]
;             [(min) 
;              (if (s 'in -inf.0)
;                  -inf.0
;      )))
;
;(define (make-natural-iset)
;  (lambda (op . args)
;    (case op
;      [(card) +inf.0]
;      [(min) 0]
;      [(max) +inf.0]
;      [(in) (>= (first args) 0)])))
;
;(define (make-natural1-iset)
;  (lambda (op . args)
;    (case op
;      [(card) +inf.0]
;      [(min) 1]
;      [(max) +inf.0]
;      [(in) (> (first args) 0)])))
;
;(define (iset-upto n m)
;  (lambda (op . args)
;    (case op
;      [(card) (+ 1 (- m n))]
;      [(min) n]
;      [(max) m]
;      [(in) (<= n (first args) m)])))
;
;(define (iset-card s)
;  (s 'card))
;
;(define (iset-in s x)
;  (s 'in x))
;
;(define (iset-min s)
;  (s 'min))
;
;(define (iset-max s)
;  (s 'max))
;
;(define (iset-union s1 s2)
;  (s1 'union s2))
;
;(define (iset-inter s1 s2)
;  (s1 'inter s2))
;
;(define (iset-setminus s1 s2)
;  (s1 'setminus s2))
;
;
;
;
;  



;; Handling of infinite sets
;; This type of arithmetic represents a set as an interval (assoc list) as follows:
;; ((x_1 x_2) (y_1 y_2) ...), 
;; where x_1 <= x_2 and x_2 < y_1, etc.

(define/contract (iset? u)
  (-> any/c boolean?)
  (or (iset-empty? u)
      (and (list? u)
           (andmap interval? u)
           ;; Another restriction is that the intervals need to be ordered 
           ;; and can't intersect.
           (let loop ([f (first u)] [r (rest u)])
             (if (iset-empty? r) 
                 #t
                 (and (< (high f) (low (first r)))
                      (loop (first r) (rest r))))))))

(define/contract (iset-empty? u)
  (-> any/c boolean?)
  (null? u))

;; Constants
(define/contract (make-integer-iset)
  (-> iset?)
  (list (cons -inf.0 +inf.0)))

(define/contract (make-natural-iset)
  (-> iset?)
  (list (cons 0 +inf.0)))

(define/contract (make-natural1-iset)
  (-> iset?)
  (list (cons 1 +inf.0)))

(define/contract (make-empty-iset)
  (-> iset-empty?)
  '())

(define/contract (iset= a b)
  (-> iset? iset? boolean?)
  (or (and (null? a) (null? b))
      (and (int= (first a) (first b))
           (iset= (rest a) (rest b)))))

(define/contract (interval->iset i)
  (-> interval? iset?)
  (list i))

(define/contract (interval-lst->iset l)
  (-> (listof interval?) iset?)
  l)

(define/contract (iset-set . els)
  (->* () () #:rest (listof integer-or-inf?) iset?)
  (if (null? els)
      (make-empty-set)
      (let ([sorted-els (sort (remove-duplicates els) <)])
        (map (lambda (el) (cons el el)) sorted-els))))
    
(define/contract (iset-in el set)
  (-> integer-or-inf? iset? boolean?)
  (and (not (untyped-empty-set? set))
       (ormap (lambda (int) ;; receives an interval from the set
                (<= (car int) el (cdr int)))
              set)))

(define/contract (iset-card set)
  (-> iset? integer-or-inf?)
  (foldl (lambda (int acum)
           (+ 1 acum (- (cdr int) (car int))))
         0
         set))
  
;; Only works over non-empty sets
(define/contract (iset-min s)
  (-> iset? integer-or-inf?)
  (caar s))

(define/contract (iset-max s) 
  (-> iset? integer-or-inf?)
  (cdar (reverse s)))

(define/contract (set-int-merge s)
  (-> (listof interval?) iset?)
  (if (null? s)
      s
      (let loop ([merged-s '()] [current-el (first s)] [rest-s (rest s)])
        (cond [(null? rest-s)
               (reverse (cons current-el merged-s))]
              [(<= (low (first rest-s))
                   (+ (high current-el) 1))
               (loop merged-s 
                     (cons (min (low current-el) (low (first rest-s)))
                           (max (high current-el) (high (first rest-s))))
                     (rest rest-s))]
              [else
               (loop (cons current-el merged-s)
                     (first rest-s)
                     (rest rest-s))]))))
            
(define/contract (iset-union a b)
  (-> iset? iset? iset?)
  (set-int-merge (sort (append a b)
                       (lambda (inta intb)
                         (< (car inta) (car intb))))))

(define/contract (iset-union* . sets)
  (->* () () #:rest (listof iset?) iset?)
  (foldl (lambda (s acum) (iset-union acum s))
         (make-empty-iset)
         sets))

(define/contract (iset-inter a b)
  (-> iset? iset? iset?)
  (if (or (iset-empty? a) (iset-empty? b))
      (make-empty-iset)
      (set-int-merge 
       (let loop ([cura (first a)] [arest (rest a)] [brest b] [result (make-empty-iset)])
         (let ([interval-int (append-map (lambda (ib) ;; interval from b
                                           (int-inter cura ib))
                                         brest)])
           (cond [(null? arest)
                  (append result interval-int)]
                 [(null? brest)
                  (loop (first arest) (rest arest) b
                        (append result interval-int))]
                 [else
                  (loop cura arest (rest brest)
                        (append result interval-int))]))))))

(define/contract (iset-inter* . sets)
  (->* () () #:rest (listof iset?) iset?)
  (foldl (lambda (s acum) (iset-inter acum s))
         (make-integer-iset)
         sets))

;; Given an element x and a set s, such that 'x in s'
;; returns y >= x such that 'y in s' and '(x .. y) in s' but 
;; '(y + 1) !in s'
(define/contract (set-highest-in-seq x s)
  (-> integer-or-inf? iset? integer-or-inf?)
  ;; We need to find the interval where x is located in s
  ;; and return the maximum of that set
  (cdr (set-find-interval x s)))

(define/contract (set-find-interval x s)
  (-> integer-or-inf? iset? interval?)
  (let ([int-or-null (filter (lambda (int) (int-in x int)
                             s))])
    (if int-or-null
        (first int-or-null)
        int-or-null)))

(define/contract (iset-setminus a b)
  (-> iset? iset? iset?)
  ;; Intervals:
  ;; (2 . 5) - (2 . 5) = ()
  ;; (2 . 10) - (4 . 5) = ((2 . 3) (6 . 10))
  ;; (2 . 10) - (0 . 15) = ()
  ;; For each of the intervals in a or b, we need to make sure they do not interset.
  (set-int-merge 
   (append-map (lambda (int-a)
                 (append-map (lambda (int-b)
                               (int-minus int-a int-b))
                             b))
               a)))

(define (iset-setminus* . sets)
  (->* () () #:rest (listof iset?) iset?)
  (cond [(null? sets) (make-empty-iset)]
        [(= (length sets) 1) (first sets)]
        [else
         (foldl (lambda (s acum) (iset-setminus acum s))
                (first sets)
                (rest sets))]))

(define/contract (iset-upto n m) 
  (-> integer? integer? iset?)
  (list (cons n m)))

; iset-upto where args can be inf.
(define (iset-upto+ n m)
  (-> integer-or-inf? integer-or-inf? iset?)
  (list (cons n m)))

;                                                          
;                                                          
;                                                          
;    ;;;;                        ;;;;          ;;          
;   ;;                          ;              ;;          
;   ;       ;;;   ;;;;          ;       ;;;   ;;;;;   ;;;  
;   ;      ;;  ;  ;;  ;         ;;     ;;  ;   ;;    ;;    
;   ;  ;;  ;   ;  ;   ;          ;;;   ;   ;   ;;    ;;    
;   ;   ;  ;;;;;  ;   ;             ;  ;;;;;   ;;     ;;;  
;   ;   ;  ;      ;   ;             ;  ;        ;        ; 
;   ;;  ;  ;      ;   ;             ;  ;        ;        ; 
;    ;;;;   ;;;;  ;   ;         ;;;;    ;;;;    ;;;  ;;;;  
;                                                          
;                                                          
;                                                          
;; Generic sets for:
;; - Other sets

(define (gset? u) (set:set? u))

(define/contract (make-empty-gset)
  (-> gset?)
  (set:make-unordered eb-equal))

(define/contract (gset-set . els)
  (->* () () #:rest (listof any/c) gset?)
  (foldl set:insert (make-empty-gset) els))

(define gset= set:equal?)

(define gset-in set:member?)
(define gset-card set:size)
(define gset-union set:union)
(define gset-inter set:intersection)
(define gset-setminus set:difference)

;                                                                               
;                                                                               
;                                        ;                                      
;    ;;;;                                                    ;;;                
;   ;;                                                      ;;  ;               
;   ;       ;;;   ;;;;    ;;;    ; ;;;  ;;      ;;;         ;   ;  ;;;;    ;;;  
;   ;      ;;  ;  ;;  ;  ;;  ;   ;;      ;     ;            ;   ;  ;;  ;  ;;    
;   ;  ;;  ;   ;  ;   ;  ;   ;   ;       ;    ;             ;   ;  ;   ;  ;;    
;   ;   ;  ;;;;;  ;   ;  ;;;;;   ;       ;    ;             ;   ;  ;   ;   ;;;  
;   ;   ;  ;      ;   ;  ;       ;       ;    ;             ;   ;  ;   ;      ; 
;   ;;  ;  ;      ;   ;  ;       ;       ;     ;            ;;  ;  ;;  ;      ; 
;    ;;;;   ;;;;  ;   ;   ;;;;   ;     ;;;;;    ;;;          ;;;   ;;;;   ;;;;  
;                                                                  ;            
;                                                                  ;            
;                                                                  ;    ; ;;    

(define/contract (eb-set? u)
  (-> any/c boolean?)
  (or (untyped-empty-set? u)
      (and (pair? u)
           (set-tag? (car u))
           (or (iset? (cdr u))
               (gset? (cdr u))))))
  
(define/contract (attach-tag set tag)
  (-> (or/c iset? gset?) (one-of/c 'int 'gen) eb-set?)
  (cons tag set))

(define/contract (set-tag? u)
  (-> any/c boolean?)
  (and (symbol? u)
       (or (eq? u 'int)       ;; Set contains integers
           (eq? u 'gen))))  ;; Set contains other element types

(define (untyped-empty-set? s)
  (-> any/c boolean?)
  (eq? s 'empty-set))

(define/contract (make-empty-set) 
  (-> untyped-empty-set?)
  'empty-set)

(define (coerce-if-empty set type)
  (if (eq? set 'empty-set)
      (make-empty-set-type type)
      set))
      
(define/contract (make-empty-set-type type)
  (-> set-tag? eb-set?)
  (cons type
        (if (eq? type 'int)
            (make-empty-iset)
            (make-empty-gset))))

(define/contract (make-integer-set)
  (-> eb-set?)
  (attach-tag (make-integer-iset) 'int))
(define/contract (make-natural-set) 
  (-> eb-set?)
  (attach-tag (make-natural-iset) 'gen))
(define/contract (make-natural1-set)
  (-> eb-set?)
  (attach-tag (make-natural1-iset) 'gen))

(define/contract (eb-bunion s1 s2)
  (-> eb-set? eb-set? eb-set?)
  (cond [(untyped-empty-set? s1) s2]
        [(untyped-empty-set? s2) s1]
        [else
         (let-values ([(tag1 set1) (car+cdr s1)]
                      [(tag2 set2) (car+cdr s2)])
           (cons tag1 
                 (if (eqv? tag1 'int)
                     (iset-union set1 set2)
                     (gset-union set1 set2))))]))
  
(define/contract (eb-binter s1 s2)
  (-> eb-set? eb-set? eb-set?)
  (cond [(untyped-empty-set? s1) s1]
        [(untyped-empty-set? s2) s2]
        [else
         (let-values ([(tag1 set1) (car+cdr s1)]
                      [(tag2 set2) (car+cdr s2)])
           (if (eqv? tag1 'int)
               (iset-inter set1 set2)
               (gset-inter set1 set2)))]))

(define/contract (eb-setminus s1 s2)
  (-> eb-set? eb-set? eb-set?)
  (if (or (untyped-empty-set? s1)
          (untyped-empty-set? s2))
      s1
      (let-values ([(tag1 set1) (car+cdr s1)]
                   [(tag2 set2) (car+cdr s2)])
        (if (eqv? tag1 'int)
            (iset-setminus set1 set2)
            (gset-setminus set1 set2)))))
      
(define/contract (eb-in x s)
  (-> any/c eb-set? boolean?)
  (if (untyped-empty-set? s)
      #f
      (if (eqv? (car s) 'int)
          (iset-in x (cdr s))
          (gset-in x (cdr s)))))

(define/contract (eb-upto n m)
  (-> integer? integer? eb-set?)
  (attach-tag (iset-upto n m) 'int))

(define/contract (list->eb-set xs)
  (-> (listof any/c) eb-set?)
  (apply eb-set xs))

(define/contract (eb-set . els)
  (->* () () #:rest (listof any/c) eb-set?)
  (if (null? els)
      (make-empty-set)
      (if (andmap integer? els)
          (attach-tag (apply iset-set els) 'int)
          (attach-tag (apply gset-set els) 'gen))))

(define/contract (set= s1 s2)
  (-> eb-set? eb-set? boolean?)
  (cond [(untyped-empty-set? s1) (untyped-empty-set? s2)]
        [(untyped-empty-set? s2) (untyped-empty-set? s1)]
        [else
         (let-values ([(tag1 set1) (car+cdr s1)]
                      [(tag2 set2) (car+cdr s2)])
           (and (eqv? tag1 tag2)
                (if (eqv? tag1 'int)
                    (iset= set1 set2)
                    (gset= set1 set2))))]))

(define (eb-equal a b)
  (-> any/c any/c boolean?)
  (letrec ([integer-equal? =]
           [pair-equal? (lambda (p1 p2) (and (eb-equal (car p1) (car p2))
                                             (eb-equal (cdr p1) (cdr p2))))]
           [set-equal? set:equal?])
    (or (and (boolean-symbol? a) (boolean-symbol? b) (eq? a b))
        (and (set-literal-symbol? a) (set-literal-symbol? b) (eq? a b))
        (and (integer? a) (integer? b) (integer-equal? a b))
        (and (pair? a) (pair? b) (pair-equal? a b))
        (and (set:set? a) (set:set? b) (set-equal? a b)))))

(define eb-notequal (compose not eb-equal))

;                                                                        
;                                                                        
;                            ;    ;                                      
;   ;;;;;                    ;                         ;                 
;   ;    ;                   ;                         ;                 
;   ;    ;  ; ;;   ;;;    ;;;;  ;;;     ;;;    ;;;   ;;;;;   ;;;    ;;;  
;   ;    ;  ;;  ; ;;  ;  ;; ;;    ;    ;;  ;  ;   ;    ;    ;;  ;  ;   ; 
;   ;;;;;   ;     ;   ;  ;   ;    ;    ;          ;    ;    ;   ;  ;     
;   ;       ;     ;;;;;  ;   ;    ;    ;       ;;;;    ;    ;;;;;   ;;;  
;   ;       ;     ;      ;   ;    ;    ;      ;   ;    ;    ;          ; 
;   ;       ;     ;;  ;  ;  ;;    ;    ;;  ;  ;   ;    ;    ;;  ;  ;   ; 
;   ;       ;      ;;;    ;;;;  ;;;;;   ;;;    ;; ;    ;;;   ;;;    ;;;  
;                                                                        
;                                                                        
;                                                                       ;

(define (eb-not a) (not a))
(define (eb-limp a b) (or (not a) b))
(define eb-leqv eq?) 

;                                                          
;                                                          
;                                                          
;   ;;;;;   ;;;;   ;;;;  ;      ;;;;;;   ;;   ;;   ;  ;;;; 
;   ;    ;  ;  ;   ;  ;  ;      ;        ;;   ;;   ; ;;   ;
;   ;    ; ;    ; ;    ; ;      ;        ;;   ; ;  ; ;     
;   ;    ; ;    ; ;    ; ;      ;       ;  ;  ; ;  ; ;;    
;   ;;;;;  ;    ; ;    ; ;      ;;;;;;  ;  ;  ; ;; ;  ;;;; 
;   ;    ; ;    ; ;    ; ;      ;       ;  ;  ;  ; ;     ;;
;   ;    ; ;    ; ;    ; ;      ;       ;;;;  ;  ; ;      ;
;   ;    ;  ;  ;   ;  ;  ;      ;      ;    ; ;   ;; ;    ;
;   ;;;;;   ;;;;   ;;;;  ;;;;;; ;;;;;; ;    ; ;   ;;  ;;;; 
;                                                          
;                                                          
;                                                          


;                                                          
;                                                          
;                                                          
;   ;;;;;  ;;   ;;;;;;;; ;;;;;;   ;;;  ;;;;;; ;;;;;   ;;;; 
;     ;    ;;   ;   ;    ;       ;   ; ;      ;   ;; ;;   ;
;     ;    ; ;  ;   ;    ;      ;      ;      ;    ; ;     
;     ;    ; ;  ;   ;    ;      ;      ;      ;    ; ;;    
;     ;    ; ;; ;   ;    ;;;;;; ;   ;; ;;;;;; ;;;;;   ;;;; 
;     ;    ;  ; ;   ;    ;      ;    ; ;      ;   ;      ;;
;     ;    ;  ; ;   ;    ;      ;    ; ;      ;    ;      ;
;     ;    ;   ;;   ;    ;       ;   ; ;      ;    ; ;    ;
;   ;;;;;  ;   ;;   ;    ;;;;;;   ;;;  ;;;;;; ;     ; ;;;; 
;                                                          
;                                                          
; 
(define (eb-plus a b) 
  (when (or (not (number? a))
            (not (number? b)))
    (printf "error in eb-plus: ~a ~a~n" a b))
  (+ a b))
(define eb-uminus -)
(define eb-minus -)
(define eb-mul *)
(define eb-div quotient)
(define eb-mod modulo)

(define eb-lt <)
(define eb-le <=)
(define eb-gt >)
(define eb-ge >=)

;                                     
;                                     
;                                     
;   ;;;;;    ;;   ;;;;;  ;;;;;   ;;;; 
;   ;    ;   ;;     ;    ;   ;; ;;   ;
;   ;    ;   ;;     ;    ;    ; ;     
;   ;    ;  ;  ;    ;    ;    ; ;;    
;   ;;;;;   ;  ;    ;    ;;;;;   ;;;; 
;   ;       ;  ;    ;    ;   ;      ;;
;   ;       ;;;;    ;    ;    ;      ;
;   ;      ;    ;   ;    ;    ; ;    ;
;   ;      ;    ; ;;;;;  ;     ; ;;;; 
;                                     
;                                     
;                                     

(define eb-mapsto cons)
;                              
;                              
;                              
;    ;;;;  ;;;;;;;;;;;;;  ;;;; 
;   ;;   ; ;        ;    ;;   ;
;   ;      ;        ;    ;     
;   ;;     ;        ;    ;;    
;    ;;;;  ;;;;;;   ;     ;;;; 
;       ;; ;        ;        ;;
;        ; ;        ;         ;
;   ;    ; ;        ;    ;    ;
;    ;;;;  ;;;;;;   ;     ;;;; 
;                              
;                              
;


(define set-mapsto cons)


;                                                                                             
;                                                                                             
;                                                                                             
;    ;;;;  ;;;;;;;;;;;;;  ;;;;          ;;;;  ;;;;;;        ;;;;;    ;;   ;;;;;  ;;;;;   ;;;; 
;   ;;   ; ;        ;    ;;   ;         ;  ;  ;             ;    ;   ;;     ;    ;   ;; ;;   ;
;   ;      ;        ;    ;             ;    ; ;             ;    ;   ;;     ;    ;    ; ;     
;   ;;     ;        ;    ;;            ;    ; ;             ;    ;  ;  ;    ;    ;    ; ;;    
;    ;;;;  ;;;;;;   ;     ;;;;         ;    ; ;;;;;;        ;;;;;   ;  ;    ;    ;;;;;   ;;;; 
;       ;; ;        ;        ;;        ;    ; ;             ;       ;  ;    ;    ;   ;      ;;
;        ; ;        ;         ;        ;    ; ;             ;       ;;;;    ;    ;    ;      ;
;   ;    ; ;        ;    ;    ;         ;  ;  ;             ;      ;    ;   ;    ;    ; ;    ;
;    ;;;;  ;;;;;;   ;     ;;;;          ;;;;  ;             ;      ;    ; ;;;;;  ;     ; ;;;; 
;                                                                                             
;                                                                                             
;                                                                                             
(define (eb-dom r) (set:map car r))
(define (eb-ran r) (set:map cdr r))
(define (eb-converse r) (set:map (lambda (p) (cons (cdr p) (car p))) r))
(define (eb-relimage r e)
  (let ([res (set:filter (lambda (pair) (eb-equal e (car pair))) r)])
    (set:map cdr res)))

(define (eb-funimage f e)
  (let ([res (eb-relimage f e)])
    (if ((set:size res) . = . 1)
        (set:select res)
        (error 'eb-funimage "Application of funimage should produce only one element but ;doesn't."))))

; r ovl s = ((dom s) domsub r) bunion s 
(define (eb-ovl r s)
  (((eb-dom s) . eb-domsub . r) . eb-bunion . s))

(define (eb-domsub s r)
  (set:filter (match-lambda 
                ((cons x y)
                 (not (set:member? x s))))
              r))

(define (eb-ransub r s)
  (set:filter (match-lambda
                ((cons x y)
                 (not (set:member? y s))))
              r))

(define (eb-domres s r)
  (set:filter (match-lambda 
                ((cons x y)
                 (set:member? x s)))
              r))

(define (eb-ranres r s)
  (set:filter (match-lambda 
                ((cons x y)
                 (set:member? y s)))
              r))
  

;                                                                                      
;                                                                                      
;                                                                                      
;                               ;                           ;;;                        
;  ;;;;;;;                      ;                             ;                        
;     ;                         ;                             ;                        
;     ;     ;;;           ;;;   ;;;;          ;    ;  ;;;     ;    ;   ;   ;;;    ;;;; 
;     ;    ;   ;         ;   ;  ;   ;         ;   ;      ;    ;    ;   ;  ;   ;  ;   ; 
;     ;    ;   ;; ;;;;;  ;   ;  ;   ;; ;;;;;  ;;  ;      ;    ;    ;   ;  ;   ;  ;;    
;     ;    ;    ;        ;;;;;  ;    ;         ;  ;   ;;;;    ;    ;   ;  ;;;;;    ;;; 
;     ;    ;    ;        ;      ;   ;;         ; ;   ;   ;    ;    ;   ;  ;          ; 
;     ;    ;;  ;         ;      ;   ;           ;;   ;   ;    ;    ;   ;  ;      ;   ; 
;     ;     ;;;           ;;;;  ;;;;            ;    ;;;;;  ;;;;;   ;;;;   ;;;;   ;;;; 
;                                                                                      
;                                                                                      
;                                                                                      
;; This function is used to transform the list of values returned by the type-list-enumerator
;; into event-b values. For example, type-list-enumerator return '() for the empty list but we 
;; well know this is not the empty-list eb-wise.
(provide to-eb-values)
(define (to-eb-values lst)
  (map (lambda (elt)
         (cond [(or (number? elt) (symbol? elt)) elt]
               [(list? elt) (list->eb-set elt)]
               [else (error 'to-eb-values "unexpected value in list: ~a" elt)]))
       lst))
             

;                              
;                              
;                        ;;;   
;   ;;;;;;                 ;   
;   ;                      ;   
;   ;      ;   ;   ;;;     ;   
;   ;      ;   ;  ;   ;    ;   
;   ;;;;;;  ; ;       ;    ;   
;   ;       ; ;    ;;;;    ;   
;   ;       ; ;   ;   ;    ;   
;   ;        ;    ;   ;    ;   
;   ;;;;;;   ;     ;; ;     ;; 
;                              
;                              
;                          ;   

(provide eval-expression
         eval-predicate)

(define-namespace-anchor a)
(define this-namespace (namespace-anchor->namespace a))

(define (integer-symbol? s)
  (eq? s 'integer))
(define (natural-symbol? s)
  (eq? s 'natural))
(define (natural1-symbol? s)
  (eq? s 'natural1))
(define (var-symbol? s)
  (and (symbol? s) (string-prefix? "var:" (symbol->string s))))
(define (const-symbol? s)
  (and (symbol? s) (string-prefix? "const:" (symbol->string s))))
(define (set-literal-symbol? s)
  (and (symbol? s) (string-prefix? "slit:" (symbol->string s))))
(define (boolean-symbol? s)
  (and (symbol? s) (or (eq? s 'true) (eq? s 'false))))
(define (set? s)
  (and (symbol? s) (string-prefix? "set:" (symbol->string s))))
(define (emptyset-symbol? s)
  (and (symbol? s) (eq? s 'emptyset)))

(define eval-expression
  (case-lambda 
    [(sexpr state1 state2 . states)
     (eval-expression sexpr (apply dict-merge state1 state2 states))]
    [(sexpr state)
     (fprintf (current-error-port) "eval-expression: ~a~nstate: ~a~n~n" sexpr state)
     (cond [(list? sexpr)
            (apply (eval (first sexpr) this-namespace)
                   (map (lambda (se) (eval-expression se state)) (rest sexpr)))]
           [(integer-symbol? sexpr) (make-integer-set)]
           [(natural-symbol? sexpr) (make-natural-set)]
           [(natural1-symbol? sexpr) (make-natural1-set)]
           [(integer? sexpr) sexpr]
           [(or (var-symbol? sexpr) (const-symbol? sexpr)) (dict-ref state sexpr)]
           [(or (set-literal-symbol? sexpr) (boolean-symbol? sexpr)) sexpr]
           ;; Sets evaluate to their set enumeration definition
           [(set? sexpr) (list->eb-set (dict-ref state sexpr))]
           [(emptyset-symbol? sexpr) (make-empty-set)]
           [else (error 'eval-expression "Unexpected expression to evaluate: ~a" sexpr)])]))

;; Implements lazy evaluation of predicates for 'and and 'or
;; - 'and only evaluates its arguments till it finds a false
;; - 'or only evaluates its arguments till it finds a true
;; 
(define eval-predicate 
  (case-lambda
    [(sexpr state1 state2 . states)
     (eval-predicate sexpr (apply dict-merge state1 state2 states))]
    [(sexpr state)
     (fprintf (current-error-port) "eval-predicate: ~a~nstate: ~a~n~n" sexpr state)
     (cond [(and (list? sexpr) (eq? (first sexpr) 'eb-land))
            (and (eval-predicate (second sexpr) state)
                 (eval-predicate (third sexpr) state))]
           [(and (list? sexpr) (eq? (first sexpr) 'eb-lor))
            (or (eval-predicate (second sexpr) state)
                (eval-predicate (third sexpr) state))]
           [(list? sexpr)
            (apply (eval (first sexpr) this-namespace)
                   (if (memq (first sexpr) '(eb-in eb-notin eb-subset eb-subseteq eb-notsubset eb-notsubseteq eb-equal eb-notequal eb-lt eb-gt eb-leq eb-geq))  
                       (map (lambda (se) (eval-expression se state)) (rest sexpr))
                       (map (lambda (se) (eval-predicate se state)) (rest sexpr))))]
           [(boolean? sexpr) sexpr]
           [else (eval-expression sexpr state)])]))


;                                                                                
;                                                     ;                          
;    ;;;;  ;           ;                ;;;;;                    ;               
;   ;;   ; ;           ;                ;   ;;                   ;               
;   ;    ;;;;;  ;;;; ;;;;;   ;;;        ;    ; ;;;; ;;;   ;;;; ;;;;;   ;;;   ;;;;
;    ;;;   ;        ;  ;    ;   ;       ;   ;; ;; ;   ;   ;   ;  ;    ;   ;  ;; ;
;      ;;; ;     ;;;;  ;    ;;;;;       ;;;;;  ;      ;   ;   ;  ;    ;;;;;  ;   
;        ; ;    ;   ;  ;    ;           ;      ;      ;   ;   ;  ;    ;      ;   
;   ;    ; ;    ;  ;;  ;    ;;          ;      ;      ;   ;   ;  ;    ;;     ;   
;    ;;;;  ;;;   ;;;;  ;;;   ;;;;       ;      ;    ;;;;; ;   ;  ;;;   ;;;;  ;   
;                                                                                
;                                                                                
;                                                                                


(define (print-state state (out (current-output-port)))
  (letrec ([print-value
            (lambda (val (out (current-output-port)))
              (cond [(or (integer? val) (symbol? val)) (fprintf out "~a" val)]
                    [(pair? val) 
                     (print-value (car val) out)
                     (fprintf out " |-> ")
                     (print-value (cdr val) out)]
                    [(set:set? val)
                     (fprintf out "{~a}"
                              (apply string-append 
                                     (add-between (map (lambda (v)
                                                         (let ([o (open-output-string)])
                                                           (print-value v o)
                                                           (get-output-string o)))
                                                       (set:elements val))
                                                  ", ")))]
                    [else
                     (error 'print-state "Can't print value of state: " val)]))])
    (parameterize ([current-output-port out])
      (for-each (match-lambda ((cons var val) 
                               (printf "~a : " var)
                               (print-value val out)
                               (newline out)))
                state))))