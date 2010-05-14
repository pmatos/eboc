#lang scheme


;                                                                        
;                                                                        
;                                                                        
;                                        ;;;                             
;   ;;;;                                ;                                
;     ;            ;                    ;                                
;     ;    ;;;;   ;;;;    ;;;   ; ;;;  ;;;;    ;;;    ;;;    ;;;    ;;;  
;     ;    ;   ;   ;     ;   ;  ;;      ;         ;  ;   ;  ;   ;  ;     
;     ;    ;   ;   ;     ;;;;;  ;       ;      ;;;;  ;      ;;;;;   ;;   
;     ;    ;   ;   ;     ;      ;       ;     ;   ;  ;      ;         ;; 
;     ;    ;   ;   ;     ;      ;       ;     ;  ;;  ;      ;          ; 
;   ;;;;   ;   ;    ;;;   ;;;   ;       ;     ;;;;;   ;;;    ;;;   ;;;;  
;                                                                        
;                                                                        
;                                                                        


(define iValue
  (interface () in equals notequals))

(define iNumber 
  (interface (iValue)
    geq leq lt gt
    plus minus mult div mod
    upto))

(define iSet
  (interface (iValue)
    contains))


;                                                                 
;                                                                 
;                                                                 
;                            ;                ;         ;         
;   ;;;;                     ;          ;;;   ;                   
;   ;   ;                    ;         ;   ;  ;                   
;   ;   ;  ; ;;;   ;;;    ;;;;         ;   ;  ;;;;    ;;;    ;;;  
;   ;   ;  ;;     ;   ;  ;   ;        ;    ;  ;   ;     ;   ;     
;   ;;;;   ;      ;;;;;  ;   ;        ;    ;  ;   ;     ;    ;;   
;   ;      ;      ;      ;   ;         ;   ;  ;   ;     ;      ;; 
;   ;      ;      ;      ;  ;;         ;   ;  ;   ;     ;       ; 
;   ;      ;       ;;;    ;;;;          ;;;   ;;;;      ;   ;;;;  
;                                                       ;         
;                                                    ;;;          
;                                                                 

(define (ebor a b)
  (or a b))

(define (eband a b)
  (and a b))

(define (ebnot a)
  (not a))

;                                                   
;                                                   
;                                                   
;          ;         ;                              
;    ;;;   ;                                        
;   ;   ;  ;                            ;           
;   ;   ;  ;;;;    ;;;    ;;;    ;;;   ;;;;    ;;;  
;  ;    ;  ;   ;     ;   ;   ;  ;   ;   ;     ;     
;  ;    ;  ;   ;     ;   ;;;;;  ;       ;      ;;   
;   ;   ;  ;   ;     ;   ;      ;       ;        ;; 
;   ;   ;  ;   ;     ;   ;      ;       ;         ; 
;    ;;;   ;;;;      ;    ;;;    ;;;     ;;;  ;;;;  
;                    ;                              
;                 ;;;                               
;                                                   


(define Value%
  (class object%
    (super-new)
    
    (define/public (in x)
      (send x contains this))
    
    (define/public (notequals x)
      (not (send this equals x)))))

;; This implements the class Number
(define Number%
  (class* Value%
    (iNumber)
    (super-new)
    
    (init-field num)
    
    (define (apply-to-number op b)
      (op num (send b get-num)))
    
    (define/public (get-num)
      num)
    
    ;; Relational Operators
    (define/public (equals b)
      (apply-to-number = b))
    
    (define/public (geq b)
      (apply-to-number >= b))
    
    (define/public (leq b)
      (apply-to-number <= b))
      
    (define/public (lt b)
      (apply-to-number < b))
    
    (define/public (gt b)
      (apply-to-number > b))
                   
    ;; Arithmetic Operators
    (define/public (plus b)
      ((compose number apply-to-number) + b))
    
    (define/public (mult b)
      ((compose number apply-to-number) * b))
    
    (define/public (div b)
      ((compose number apply-to-number) quotient b))
    
    (define/public (minus b)
      ((compose number apply-to-number) - b))
    
    (define/public (mod b)
      ((compose number apply-to-number) modulo b))
    
    ;; Set Operators
    (define/public (upto b)
      (implicit-set num (send b num)))))

(define ImplicitSet%
  (class object%
    (init-field min)
    (init-field max)
    
    (super-new)
    
    (define/public (implicit-set? s) #t)
    
    (define/public (equals s)
      (if (implicit-set? s)
          (eband (eb= min (ebmin s))
                 (eb= max (ebmax s)))
          (eband (eb= min (ebmin s))
                 (eb= max (ebmax s))
                 (let loop ([i min])
                   (cond [(eb> i max) #t]
                         [(ebin i s) (loop (eb+ i (number 1)))]
                         [else #f])))))
    (define (finite) #t)
    
    (define (card)
      (eb+ (number 1) (eb- max min)))
    
    (define/public (contains x)
      (and (eb<= min x) (eb<= x max)))))

(define IntegerSet%
  (class object%
    (super-new)
    
    (define/public (contains x)
      (send number? x))

    (define/public (bunion s)
      this)
    
    (define/public (binter s)
      s)))
          
(define Powerset%
  (class object%
    (super-new)
    
    (init-field set)
    
    (define/public (contains x)
      (or (eb= x emptyset)
          
          

;                                                                 
;                                                                 
;                                                                 
;          ;                                                      
;    ;;;   ;                                                      
;   ;      ;                     ;                    ;           
;   ;      ;;;;    ;;;   ; ;;;  ;;;;    ;;;   ;   ;  ;;;;    ;;;  
;    ;;    ;   ;  ;   ;  ;;      ;     ;   ;  ;   ;   ;     ;     
;      ;;  ;   ;  ;   ;  ;       ;     ;      ;   ;   ;      ;;   
;       ;  ;   ;  ;   ;  ;       ;     ;      ;   ;   ;        ;; 
;   ;   ;  ;   ;  ;   ;  ;       ;     ;      ;  ;;   ;         ; 
;   ;;;;   ;   ;   ;;;   ;        ;;;   ;;;    ;;;;    ;;;  ;;;;  
;                                                                 
;                                                                 
;                                                                 
                    
(define (number n)
  (new Number% [num n]))
                    
(define (implicit-set a b)
  (new ImplicitSet% [min a] [max b]))

(define (eb+ a b)
  (send a plus b))
  
(define (eb- a b)
  (send a minus b))

(define (eb* a b)
  (send a mult b))

(define (ebdiv a b)
  (send a div b))

(define (eb< a b)
  (send a lt b))

(define (eb= a b)
  (send a equals b))

(define (ebinv s)
  (send s inv))

(define (ebcontains s x)
  (send s contains x))

(define (ebpow s)
  (send s pow))

(define (ebsetminus s1 s2)
  (send s1 setminus s2))

(define (ebdom s)
  (send s dom))

(define (ebcprod s1 s2)
  (send s1 cprod s2))

(define (ebbinter s1 s2)
  (send s1 binter s2))

(define (ebbunion s1 s2)
  (send s1 bunion s2))

(define (ebsrel s1 s2)
  (send s1 srel s2))

(define (ebtrel s1 s2)
  (send s1 trel s2))

(define (ebfcomp s1 s2)
  (send s1 fcomp s2))

(define (ebid s)
  (send s id))

(define (ebdprod s1 s2)
  (send s1 dprod s2))

(define (ebpinj s1 s2)
  (send s1 pinj s2))

(define (ebtfun s1 s2)
  (send s1 tfun s2))

(define (ebpsur s1 s2)
  (send s1 psur s2))

(define (ebcard s)
  (send s card))

(define (ebfinite s)
  (send s finite))

(define emptyset '())

;                                                          
;                                                          
;                                                          
;                          ;;;                         ;;; 
;   ;;;;                  ;            ;;;;           ;    
;   ;   ;                 ;            ;  ;;          ;    
;   ;   ;  ; ;;;   ;;;   ;;;;          ;   ;   ;;;   ;;;;  
;   ;   ;  ;;     ;   ;   ;            ;   ;  ;   ;   ;    
;   ;;;;   ;      ;;;;;   ;            ;   ;  ;;;;;   ;    
;   ;      ;      ;       ;            ;   ;  ;       ;    
;   ;      ;      ;       ;            ;  ;;  ;       ;    
;   ;      ;       ;;;    ;            ;;;;    ;;;    ;    
;                                                          
;                                                          
;                                                          

; existential quantifier

; implication

; conjunction

; equivalence

; true

; <=
(define (eb<= a b)
  (ebor (eb< a b)
        (eb= a b)))

; >
(define (eb> a b)
  (ebnot (eb<= a b)))

; >= 
(define (eb>= a b)
  (ebnot (eb< a b)))

; !=
(define (eb!= a b)
  (ebnot (eb= a b)))
    
; in
(define (ebin x s)
  (ebcontains s x))

; subseteq
(define (ebsubseteq x s)
  (ebin x (ebpow s)))

; subset
(define (ebsubset x s)
  (eband (eb!= x s)
         (ebsubseteq x s)))

; notin
(define (ebnotin x s)
  (ebnot (ebin x s)))

; not subseteq
(define (ebnotsubseteq x s)
  (ebnot (ebsubseteq x s)))

; not subset
(define (ebnotsubset x s)
  (ebnot (ebsubset x s)))

;                                                          
;                                                          
;                                                          
;                                                      ;;; 
;   ;;;;;                              ;;;;           ;    
;   ;                                  ;  ;;          ;    
;   ;      ;  ;   ;;;;   ; ;;;         ;   ;   ;;;   ;;;;  
;   ;;;;    ; ;   ;   ;  ;;            ;   ;  ;   ;   ;    
;   ;       ;;    ;   ;  ;             ;   ;  ;;;;;   ;    
;   ;       ;;    ;   ;  ;             ;   ;  ;       ;    
;   ;       ; ;   ;   ;  ;             ;  ;;  ;       ;    
;   ;;;;;  ;   ;  ;;;;   ;             ;;;;    ;;;    ;    
;                 ;                                        
;                 ;                                        
;                                                          


; powerset1
(define (ebpow1 s)
  (ebsetminus (ebpow s)
              emptyset))

; inter

; union

; mod
(define (ebmod a b)
  (eb- a (eb* b (ebdiv a b))))

; ran
(define (ebran s)
  (ebinv (ebdom s)))

; relation
(define (ebrel s1 s2)
  (ebpow (ebcprod s1 s2)))

; surjective total relation <<->>
(define (ebstrel s1 s2)
  (ebbinter (ebsrel s1 s2) (ebtrel s1 s2)))

; backward composition
(define (ebbcomp s1 s2)
  (ebfcomp s2 s1))

; domain restriction
(define (ebdomres s1 s2)
  (ebfcomp (ebid s1) s2))

; range restriction
(define (ebranres s1 s2)
  (ebfcomp s1 (ebid s2)))

; domain subtraction
(define (ebdomsub s1 s2)
  (ebdomres (ebsetminus (ebdom s2) s1)
            s2))

; range subtraction
(define (ebransub s1 s2)
  (ebranres s1 
            (ebsetminus (ebran s1) s2)))

; function application
(define (ebfnapply s1 s2)
  (ebran (ebdomres s1 s2)))

; function overload
(define (ebovl s1 s2)
  (ebbunion (ebdomsub (ebdom s2) s1) s2))

; project1
(define (ebprjone s1 s2)
  (ebinv (ebdprod (ebid s1)
                  (ebcprod s1 s2))))

; project2
(define (ebprjtwo s1 s2)
  (ebinv (ebdprod (ebcprod s1 s2)
                  (ebid s2))))

; Total Injection
(define (ebtinj s1 s2)
  (ebbinter (ebpinj s1 s2)
            (ebtfun s1 s2)))

; Total surjection
(define (ebtsur s1 s2)
  (ebbinter (ebpsur s1 s2)
            (ebtfun s1 s2)))

; Total Bijection
(define (ebtbij s1 s2)
  (ebbinter (ebtinj s1 s2)
            (ebtsur s1 s2)))