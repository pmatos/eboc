#lang scheme

;; This file implements the scheduler that does the search over the state space

(require (prefix-in pq: "alg/priority-queue.ss")
         (prefix-in set: (planet soegaard/galore:4:1/set))
         "eventb-lib.scm"
         "probabilities.scm")

(provide register-initialisation!
         register-event!
         register-property!
         search!)

(define-struct search-state
  (inits           ;; generator procedure
   events          ;; List of lists (name guard-proc action-proc)
   props           ;; List of pairs (name prop-proc)
   queue           ;; Search queue of action-proc
   states-checked) ;; Number of states checked
  #:mutable)

(define current-search-state (make-parameter (make-search-state '() '() '() (pq:empty) 0)))

(define (register-initialisation! proc)
  (set-search-state-inits! (current-search-state) proc))

(define (register-event! name guard-proc actions-proc)
  (set-search-state-events! (current-search-state)
                            (cons (list name guard-proc actions-proc)
                                  (search-state-events (current-search-state)))))

(define (register-property! prop-predicate #:name (name 'undef))
  (set-search-state-props! (current-search-state)
                           (cons (cons name prop-predicate) (search-state-props (current-search-state)))))

;; Inserts a generator into the queue with a priority whose value is computed with a mean around the priority
(define (search-queue-insert! state-gen priority)
  (unless (and (>= priority 0.0)
               (<= priority 1.0))
    (error 'search-queue-insert! "Unexpected priority which should be in [0.0, 1.0]: ~a" priority))
  (letrec ([insert/now! 
            (lambda (prt) 
              (set-search-state-queue! (current-search-state)
                                       (pq:insert state-gen
                                                  prt
                                                  (search-state-queue (current-search-state)))))]
           [get-new-prt
            (lambda ()
              (let ([variation (/ (random) 1000000.0)]
                    [sgn-variation (random 2)])
                (cond [(priority . <=  . 0.0001) (+ priority variation)]
                      [(priority . >= . 0.9999) (- priority variation)]
                      [else 
                       (if (zero? sgn-variation)
                           (- priority variation)
                           (+ priority variation))])))])
    (insert/now! (- 1.0 (get-new-prt)))))

;                                                                               
;                                                                               
;    ;; ;                             ;;             ;;;                        
;   ;  ;;                              ;              ;                         
;   ;       ;;;    ;;;   ;; ;;   ;;;;  ; ;;           ;      ;;;    ;;;  ;; ;;  
;    ;;;   ;   ;  ;   ;   ;;    ;   ;  ;;  ;          ;     ;   ;  ;   ;  ;;  ; 
;       ;  ;;;;;   ;;;;   ;     ;      ;   ;          ;     ;   ;  ;   ;  ;   ; 
;       ;  ;      ;   ;   ;     ;      ;   ;          ;  ;  ;   ;  ;   ;  ;   ; 
;   ;;  ;  ;      ;   ;   ;     ;   ;  ;   ;          ;  ;  ;   ;  ;   ;  ;   ; 
;   ; ;;    ;;;;   ;;;;; ;;;;;   ;;;  ;;; ;;;        ;;;;;   ;;;    ;;;   ;;;;  
;                                                                         ;     
;                                                                        ;;;    
;                                                                               
;                                                                               

;; This is the main search procedure. 
;; It expands states and adds them to a queue
#|

How does the search work?
------------------------

n states will be checked for property violations. If a property is violated, search! will return (listof (cons violated-property (listof event-name))).
If after checking n states no property is violated it returns null.

- [1] All initialisation generators (with empty path) are added to queue by priority.
- [2] Loop until number of states checked is reached or the queue is empty (meaning we search the complete search space)
-- [3] Remove generator with lowest priority          
-- [4] Generate state
-- [5] Add one to number of checked states
-- [6] Check if it is a non-det generator, if it is reenter it into queue with new priority and same path, otherwise discard
-- [7] Check if state violates property, if it does return property name and the reversed path attached to the generator
-- [8] Check which guards return an enabled generator for this new state and add them all to the queue with their advertised priority
|#  

(define (search! n #:debug? (debug? #f) #:cache-states? (hash? #t))
  (let ([begin-secs (current-inexact-milliseconds)]
        [graph-proc (if debug? (create-search-graph) #f)]
        [data-proc (if debug? (create-data-plot) #f)]
        [state-cache (if hash? (set:make-equal) #f)])
    (letrec ([print-time-info
              (lambda ()
                (printf "---> Speed: ~a states/sec~n" (/ (search-state-states-checked (current-search-state)) (/ (- (current-inexact-milliseconds) begin-secs) 1000))))])
      
      (let ([init-guard (search-state-inits (current-search-state))])
        (search-queue-insert! (cons '() init-guard) (init-guard 'prt)))
      
      (let loop ([violations '()])
        (cond  [(not (null? violations))
                (when debug? 
                  (printf "Graph written to ~a~n" (graph-proc 'end))
                  (printf "Data written to ~a~n" (data-proc 'end)))
                (print-time-info) 
                (printf "Model checking finished. VIOLATION FOUND~n")
                violations]
               [(> (search-state-states-checked (current-search-state)) n)
                (when debug? 
                  (printf "Graph written to ~a~n" (graph-proc 'end))
                  (printf "Data written to ~a~n" (data-proc 'end)))
                (print-time-info) 
                (printf "Model checking finished without finding any violation for ~a states.~n" n)
                '()]
               [(pq:empty? (search-state-queue (current-search-state)))
                (when debug? 
                  (printf "Graph written to ~a~n" (graph-proc 'end))
                  (printf "Data written to ~a~n" (data-proc 'end)))
                (print-time-info)
                (printf "Model checking finished exploring all available states.~nSearch space exhausted: NO VIOLATION~n")
                '()]
               [else
                ;;; Print queue status
                ;; 
                ;(let-values ([(els prt) (pq:elements+priorities (search-state-queue (current-search-state)))])                 
                ;  (printf "~n~nQueue size: ~a, states checked: ~a~n" (pq:size (search-state-queue (current-search-state))) (search-state-states-checked (current-search-state)))
                ;  (printf "Queue: ~a els, prts: ~a~n" (length prt) (map (lambda (x) (- 1.0 x)) prt)))
                
                
                (let* ([low-prt-prt (- 1.0 (pq:find-min-priority (search-state-queue (current-search-state))))]
                       [low-prt-el (begin0
                                     (pq:find-min (search-state-queue (current-search-state)))
                                     (set-search-state-queue! (current-search-state) (pq:delete-min (search-state-queue (current-search-state)))))]
                       [low-prt-path (car low-prt-el)]
                       [low-prt-gen (cdr low-prt-el)])
                  
                  
                  
                  (if (low-prt-gen 'empty?)
                      (loop '())
                      
                      (let ([newstate (low-prt-gen 'stt)]
                            [new-prt (low-prt-gen 'prt)])
                        ;(printf "~nGot new state ~a with priority ~a.~n" newstate new-prt)
                        
                        (search-queue-insert! low-prt-el new-prt)
                        
                        (if (not newstate)
                            (loop '())
                            (begin 
                              
                              ;(printf "Current-state: ~a~n" newstate)
                              ;(printf "Current-path: ~a~n" low-prt-path)
                              
                              ;; Write to graph
                              (when debug?
                                (graph-proc 'edge (low-prt-gen 'pre-stt) (low-prt-gen 'ev-name) newstate (search-state-states-checked (current-search-state))))
                              
                              (if (and hash? (set:member? newstate state-cache))
                                  (loop'())
                                  
                                  (let ([violated-properties 
                                         (foldl (lambda (prop-pair acum)
                                                  (match prop-pair 
                                                    ((cons name pred) 
                                                     (if (pred newstate)
                                                         acum
                                                         (cons (cons name (reverse low-prt-path)) acum)))))
                                                '()
                                                (search-state-props (current-search-state)))])
                                    
                                    (when debug?
                                      (data-proc 'data 
                                                 (search-state-states-checked (current-search-state)) 
                                                 (- (current-inexact-milliseconds) begin-secs)
                                                 (let-values ([(_ prts) (pq:elements+priorities 
                                                                         (search-state-queue 
                                                                          (current-search-state)))])
                                                   prts)))
                                    
                                    (set-search-state-states-checked! (current-search-state) (+ 1 (search-state-states-checked (current-search-state))))
                                    
                                    (if (not (null? violated-properties))
                                        (loop violated-properties)
                                        (begin
                                          (when hash? (set! state-cache (set:insert newstate state-cache)))
                                          
                                          (for-each (match-lambda 
                                                      ((cons name guard-proc)
                                                       (let ([generator (guard-proc newstate)])
                                                         (when (not (generator 'empty?))
                                                           ;(printf "- ~a enabled~n" name)
                                                           ;(printf "~a * ~a * ~a = ~a~n" 
                                                           ;        low-prt-prt 
                                                           ;        (prob (+ 1.0 (length low-prt-path))) 
                                                           ;        (generator 'prt) 
                                                           ;        (* low-prt-prt 
                                                           ;           (prob (+ 1.0 (length low-prt-path)))
                                                           ;           (generator 'prt)))
                                                           (search-queue-insert! (cons (cons name low-prt-path) generator) 
                                                                                 (* low-prt-prt 
                                                                                    (prob (+ 1.0 (length low-prt-path))) (generator 'prt)))))))
                                                    (map (lambda (lst) (cons (first lst) (second lst))) (search-state-events (current-search-state))))
                                          (loop '()))))))))))])))))


;                                                                                                    
;                                                                                                    
;                               ;                                                  ;                 
;     ;;;                       ;               ;;;                         ;                        
;    ;   ;                      ;              ;   ;                        ;                        
;   ;       ; ;;   ;;;   ;;;;   ; ;;          ;       ; ;;   ;;;    ;;;   ;;;;;  ;;;     ;;;   ; ;;  
;   ;       ;;  ; ;   ;  ;; ;;  ;;  ;         ;       ;;  ; ;;  ;  ;   ;    ;      ;    ;; ;;  ;;  ; 
;   ;   ;;  ;         ;  ;   ;  ;   ;         ;       ;     ;   ;      ;    ;      ;    ;   ;  ;   ; 
;   ;    ;  ;      ;;;;  ;   ;  ;   ;         ;       ;     ;;;;;   ;;;;    ;      ;    ;   ;  ;   ; 
;   ;    ;  ;     ;   ;  ;   ;  ;   ;         ;       ;     ;      ;   ;    ;      ;    ;   ;  ;   ; 
;    ;   ;  ;     ;   ;  ;;  ;  ;   ;          ;   ;  ;     ;;  ;  ;   ;    ;      ;    ;; ;;  ;   ; 
;     ;;;   ;      ;; ;  ;;;;   ;   ;           ;;;   ;      ;;;    ;; ;    ;;;  ;;;;;   ;;;   ;   ; 
;                        ;                                                                           
;                        ;                                                                           
;                        ;                                                                           

(define (create-search-graph)
  (letrec ([state->string
            (lambda (state)
              (apply string-append (add-between (dict-map state (lambda (key val) (format "~a = ~a;" key val))) "\\n")))])
    (let* ([filename (make-temporary-file)]
           [fp (open-output-file filename #:exists 'replace #:mode 'text)])
      (fprintf fp "digraph {~n")
      (fprintf fp "node [shape=box];~n")
      (fprintf fp "initialisation [shape=point];~n")
      (lambda (msg . args)
        (case msg
          [(end) (begin0 filename (fprintf fp "}~n") (close-output-port fp))]
          [(edge) 
           (match args
             ((list _ '*Initialisation* poststate n)
              (fprintf fp "initialisation -> \"~a\" [label=\"(~a)\",color=red]~n" (state->string poststate) n))
             ((list prestate event-name poststate n)
              (fprintf fp "\"~a\" -> \"~a\" [label=\"~a (~a)\"]~n" (state->string prestate) (state->string poststate) event-name n)))])))))

(define (create-data-plot)
  (let* ([filename (make-temporary-file)]
         [fp (open-output-file filename #:exists 'replace #:mode 'text)])
    (fprintf fp "# Data file...~n# Cols info~n")
    (fprintf fp "#States  AccumTime QueueSz  AvgPrt  Prts~n")
    (lambda (msg . args)
      ;(printf "Calling data-plot func~nmsg: ~a~nargs: ~a~n" msg args)
      (case msg
        [(end) (begin0 filename (fprintf fp "# EOF~n") (close-output-port fp))]
        [(data)
         (match args
           ((list states accumtime prts)
            (fprintf fp "~a ~a ~a ~a "
                     states
                     accumtime
                     (length prts)
                     (exact->inexact (/ (apply + prts) (length prts))))))
         (for-each (lambda (prt-val)
                     (fprintf fp "~a " prt-val))
                   (rest args))
         (fprintf fp "~n")]))))
