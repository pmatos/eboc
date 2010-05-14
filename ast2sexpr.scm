#lang scheme

(require scheme/match
         "ast.scm"
         "types.scm"
         (only-in "utils.scm" symbol-append))

;; Generates an XML like representation based on S-expressions of an Event-B AST.

(define (type->sexpr type)
  (match type
    ((struct Type-Integer _) 'INT)
    ((struct Type-Boolean _) 'BOOL)
    ((struct Type-Enumeration (name els)) `(,name : ,(map (lambda (el) (symbol-append 'slit: el)) els)))
    ((struct Type-Powerset (subtype)) `(POW ,(type->sexpr subtype)))
    ((struct Type-CartesianProduct (subtype1 subtype2)) `(,(type->sexpr subtype1) x ,(type->sexpr subtype2)))
    ((struct Type-Polymorphic (name)) name)
    ((struct Type-UndefEnumeration (name)) `(POW ,name))))

(define (expression->sexpr expr (prefix '||) (types? #f))
  (match expr 
    ((struct Expr/wt (type expr))
     (if types?
         `(typed ,(expression->sexpr expr prefix types?) 
                 ,(type->sexpr type))
         (expression->sexpr expr prefix types?)))
    ((struct Integer-Literal (val)) val)
    ((struct Expression-Literal (lit)) lit)
    ((struct Variable (name)) (symbol-append 'var: name))
    ((struct Constant (name)) (symbol-append 'const: name))
    ((struct Set-Literal (name)) (symbol-append 'slit: name))
    ((struct Set (name)) (symbol-append 'set: name))
    ((struct Variable-Pair (car cdr)) `(,(symbol-append prefix 'mapsto) ,(expression->sexpr car prefix types?) 
                                                                        ,(expression->sexpr cdr prefix types?)))
    ((struct Expression-Bool (pred)) `(,(symbol-append prefix 'bool) ,(predicate->sexpr pred prefix)))
    ((struct Expression-UnOp (op arg)) `(,(symbol-append prefix op) ,(expression->sexpr arg prefix types?)))
    ((struct Expression-BinOp (op arg1 arg2)) `(,(symbol-append prefix op) ,(expression->sexpr arg1 prefix types?) 
                                                                           ,(expression->sexpr arg2 prefix types?)))
    ((struct Lambda-Expression (id-pat pred expr))
     `(,(symbol-append prefix 'lambda) (expression->sexpr id-pat prefix types?) 
                                       (predicate->sexpr pred prefix)
                                       (expression->sexpr expr prefix types?)))
    ((struct Set-Comprehension (op id-pat pred expr))
     `(,(symbol-append prefix op) (expression->sexpr id-pat prefix types?)
                                  (predicate->sexpr pred prefix)
                                  (expression->sexpr expr prefix types?)))
    ((struct Set-Enumeration (vals))
     `(,@(map (lambda (v) (expression->sexpr v prefix types?)) vals)))))

(define (predicate->sexpr pred (prefix '||))
  (match pred
    ((struct Predicate-Literal (lit)) 
     (if (eq? lit 'btrue)
         #t
         #f))
    ((struct Predicate-UnOp (op arg)) `(,(symbol-append prefix op) ,(predicate->sexpr arg prefix)))
    ((struct Predicate-BinOp (op arg1 arg2)) `(,(symbol-append prefix op) ,(predicate->sexpr arg1 prefix)
                                                                          ,(predicate->sexpr arg2 prefix)))
    ((struct Predicate-RelOp (op arg1 arg2)) `(,(symbol-append prefix op) ,(expression->sexpr arg1 prefix)
                                                                          ,(expression->sexpr arg2 prefix)))
    ((struct Quantifier (quant var body)) `(,(symbol-append prefix quant) ,(expression->sexpr var prefix)
                                                                       ,(predicate->sexpr body prefix)))))

(provide/contract
 [type->sexpr (type? . -> . any/c)]
 [expression->sexpr ((expression?) (symbol? boolean?) . ->* . any/c)]
 [predicate->sexpr ((predicate?) (symbol?) . ->* . any/c)])
                                             