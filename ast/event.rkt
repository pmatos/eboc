#lang scheme

(require "predexpr.scm"
         "labelled.scm"
         "action.scm"
         "../params.scm"
         "../untyped-utils.scm")

(provide pp-Event)

(define-struct Event 
  (name status locals guards actions)
  #:property prop:custom-write
  (lambda (struct port write?)
    (pp-Event struct port)))

(define (pp-Event struct (port (current-output-port)) #:tab-level (tabs 0))
  (parameterize ([current-output-port port])
    (letrec ([indent (lambda (n) (printf (make-string (* (spaces/tab) n) #\space)))])
      (match struct
        ((struct Event (name status locals guards actions))
         (indent tabs) (printf "Event ~a~n" name)
      
         (unless (eq? status 'normal)
           (indent (+ 1 tabs)) (printf "status ~a~n" status))
         
         (unless (null? locals)
           (indent (+ 1 tabs)) (printf "any~n")
           (for-each (lambda (var) (indent (+ 2 tabs)) (printf "~a" var)) locals))
         
         (unless (null? guards)
           (indent (+ 1 tabs)) (if (null? locals) (printf "when~n") (printf "where~n"))
           (for-each (lambda (g) (indent (+ 2 tabs)) (printf "~a~n" g)) guards))
           
         (unless (null? actions)
           (indent (+ 1 tabs)) (printf "then~n")
           (for-each (lambda (a) (indent (+ 2 tabs)) (printf "~a~n" a)) actions))
         
         (indent tabs) (printf "End~n"))))))

(define typed-event? 
  (match-lambda 
    ((struct Event (name status locals guards actions))
     (and (andmap typed-variable? locals)
          (andmap (compose typed-predicate? Labelled-struct) guards)
          (andmap (compose typed-action? Labelled-struct) actions)))))

(provide/contract
 [struct Event ((name symbol?)
                (status (one-of/c 'normal 'convergent))
                (locals (or/c (listof Variable?) (listof typed-variable?)))
                (guards (listof labelled-predicate?))
                (actions (listof labelled-action?)))]
 [typed-event? (Event? . -> . boolean?)])