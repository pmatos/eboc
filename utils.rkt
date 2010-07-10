#lang racket

(provide anyof?
         allof?
         symbol-append)

(define (anyof? pred-list val)
  (ormap (lambda (p) (p val)) pred-list))

(define (allof? pred-list val)
  (andmap (lambda (p) (p val)) pred-list))

(define (symbol-append . ss)
  (string->symbol (apply string-append (map symbol->string ss))))