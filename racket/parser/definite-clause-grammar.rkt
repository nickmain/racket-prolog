#lang racket
;; ------------------------------------------------------------------
;; Copyright (C) 2013, David Nick Main.
;;
;; See LICENSE file for details of license terms.
;; ------------------------------------------------------------------

;; Definite Clause Grammar AST preprocessor

(require syntax/readerr)
(require "ast.rkt")
(provide rewrite-dcg-rule)

;; Translate Definite Clause Grammar rules to normal rules.
;; Return non-DCG rules as-is
(define (rewrite-dcg-rule rule-ast)
  (match rule-ast
    [(Compound p 'DCG `(,head ,body))
     (let ([in-var  (Variable p 'dcg-in)]
           [out-var (Variable p 'dcg-out)])
       (Compound p 'RULE 
                 (list
                  (rewrite-dcg-head head in-var out-var)
                  (rewrite-dcg-body body in-var out-var))))]
    
     [_ rule-ast]))

(define (rewrite-dcg-head head in-var out-var)
  (match head
    [(Atom p symb) 
     (Compound p symb (list in-var out-var))]
    
    [(Compound p functor args)
     (Compound p functor (append args (list in-var out-var)))]
    
    [_ (let-values ([(src line col offset span) (vector->values (AST-position head))])
         (raise-read-error "Bad DCG rule head" src line col offset span))]))

(define (rewrite-dcg-body body in-var out-var)
  (match body
    ; let parens unwrapping happen in the usual rule generation
    [(Parens p term) (Parens p (rewrite-dcg-body term in-var out-var))]
    
    [(Atom p symb) (case symb
                     [(!) body]
                     [else
                      (Compound p symb (list in-var out-var))])]

    [(List p head #f)
     (Compound p 'UNIFY (list in-var (List p head out-var)))]
    
    [(Compound p 'BRACES (list term)) term] ; escape for "normal" prolog goals

    [(Compound p 'NOT-PROV (list term)) 
     (Compound p 'NOT-PROV (list (rewrite-dcg-body term in-var out-var)))] 
    
    ; if-then is same as conjunction as far as DCG rewrite is concerned
    [(Compound p 'IF-THEN `(,left ,right)) 
     (let ([intermediate-var (Variable p (gensym 'dcg-))])
       (Compound p 'IF-THEN (list (rewrite-dcg-body left in-var intermediate-var)
                             (rewrite-dcg-body right intermediate-var out-var))))]
    
    ; conjunction requires an intermediate variable to thread the output and input
    [(Compound p 'COMMA `(,left ,right))
     (let ([intermediate-var (Variable p (gensym 'dcg-))])
       (Compound p 'COMMA (list (rewrite-dcg-body left in-var intermediate-var)
                                (rewrite-dcg-body right intermediate-var out-var))))]
    
    ; disjunction uses same in/out vars for each side
    [(Compound p 'SEMI `(,left ,right))
     ((Compound p 'SEMI (list (rewrite-dcg-body left in-var out-var)
                              (rewrite-dcg-body right in-var out-var))))]
    
    [(Compound p functor args)
     (Compound p functor (append args (list in-var out-var)))]
    
    [_ (let-values ([(src line col offset span) (vector->values (AST-position body))])
         (raise-read-error "Bad DCG rule body" src line col offset span))]))
