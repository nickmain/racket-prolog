#lang racket
;; ------------------------------------------------------------------
;; Copyright (C) 2013, David Nick Main.
;;
;; See LICENSE file for details of license terms.
;; ------------------------------------------------------------------

;; A Prolog AST built by the prolog-parser

(provide (all-defined-out))

(struct AST (position))

(struct Directive AST (term) #:transparent)

(struct Term AST ())

(struct NameTerm    Term (symbol) #:transparent)
(struct Atom        NameTerm () #:transparent)
(struct Compound    NameTerm (args) #:transparent)

(struct Number     Term (value) #:transparent)
(struct Variable   Term (symbol) #:transparent)
(struct String     Term (text) #:transparent)
(struct Parens     Term (term) #:transparent)
(struct HashTable  Term (hashtable) #:transparent)
(struct List       Term (head tail) #:transparent) 
(struct Vector     Term (elements) #:transparent) 
(struct Accessor   Term (target key) #:transparent) 

(struct AnonVariable Variable () #:transparent)

;; Get the source position or #f from an AST node
(define (get-ast-posn ast-node)
  (match ast-node
    [(AST posn) posn]
    [_ #f]))