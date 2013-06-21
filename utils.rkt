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

(provide anyof?
         allof?
         symbol-append)

(define (anyof? pred-list val)
  (ormap (lambda (p) (p val)) pred-list))

(define (allof? pred-list val)
  (andmap (lambda (p) (p val)) pred-list))

(define (symbol-append . ss)
  (string->symbol (apply string-append (map symbol->string ss))))