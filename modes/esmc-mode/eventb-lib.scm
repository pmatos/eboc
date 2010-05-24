#lang scheme

;                                                                                             
;                                                                                             
;                                                                                             
;                        ;             ;;;      ;                                       ;;;   
;    ;;;                 ;               ;                         ;;;;;                  ;   
;   ;                    ;               ;                         ;                      ;   
;   ;      ;   ; ;;;;;;  ;;;;    ;;;     ;     ;;     ;;;          ;      ;   ;   ;;;     ;   
;    ;;    ;   ; ;; ; ;  ;   ;  ;   ;    ;      ;    ;   ;         ;;;;   ;   ;      ;    ;   
;      ;;   ; ;  ;  ; ;  ;   ;  ;   ;    ;      ;    ;             ;      ;; ;    ;;;;    ;   
;       ;   ; ;  ;  ; ;  ;   ;  ;   ;    ;      ;    ;             ;       ; ;   ;   ;    ;   
;   ;   ;    ;   ;  ; ;  ;   ;  ;   ;    ;      ;    ;             ;       ;;    ;  ;;    ;   
;   ;;;;     ;   ;  ; ;  ;;;;    ;;;   ;;;;   ;;;;    ;;;          ;;;;;    ;    ;;;;;  ;;;;  
;            ;                                                                                
;          ;;                                                                                 
;                                                                                             


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