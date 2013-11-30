#lang racket
;; ------------------------------------------------------------------
;; Copyright (C) 2013, David Nick Main.
;;
;; See LICENSE file for details of license terms.
;; ------------------------------------------------------------------

;; Prolog parser

(require parser-tools/yacc)
(require parser-tools/lex)
(require syntax/readerr)
(require "lexer.rkt")
(require "ast.rkt")

(provide (all-defined-out))

(define (prolog-parser src)
  (parser
   (tokens value-tokens delimiter-tokens op-tokens)
   (start Terms)
   (end EOF)
   (src-pos)
   (error (λ(ok? name val start end)
            (let ([offset (position-offset start)]
                  [offend (position-offset end)])              
              (raise-read-error (~a "Parse error: " name 
                                    (if val (~a "(" val ")") ""))
                                src 
                                (position-line start)
                                (position-col start) 
                                offset (- offend offset)))))
   
   (precs (nonassoc RULE DCG QUERY)          
          (nonassoc AT-SIGN)
          (nonassoc EQ-DOUBLE-ARROW EQ-EQ-ARROW)
          (nonassoc BACKSLASH)
          (right    BAR)
          (right    SEMICOLON)
          (right    IF-THEN)
          (right    COMMA)
          (nonassoc ASSIGN)
          (right    NOT-PROVABLE)
          (nonassoc UNIFY IS NOT-UNIFY IDENTICAL NOT-IDENT NUM-LT NUM-LE NUM-GT NUM-GE NUM-NE NUM-EQ)
          (left     MINUS PLUS)
          (left     MULT DIV)
          (nonassoc COLON)
          (left     PERIOD))
   
   (grammar 
    (Terms [(Directive Terms)     (cons $1 $2)]           
           [(QUERY Term TERM-END) (Compound (pos src $2-start-pos $2-end-pos) 'QUERY (list $2))]
           [(Term TERM-END Terms) (cons $1 $3)]
           [() null])     
    
    (Directive [(RULE Atom Term TERM-END) 
                (Directive 
                 (pos src $1-start-pos $4-end-pos) 
                 (Compound (pos src $2-start-pos $3-end-pos) 
                           (NameTerm-symbol $2) 
                           (flatten-commas $3)))]
               [(RULE Term TERM-END) 
                (Directive (pos src $1-start-pos $3-end-pos) $2)])
    
    (KeyValuePair  [(Atom COLON Term) (cons $1 $3)]) 
    (KeyValuePairs [(KeyValuePair) (list $1)]
                   [(KeyValuePair COMMA KeyValuePairs) (cons $1 $3)]) 
                               
    (Term [(Atom OPEN-PAREN Term CLOSE-PAREN) 
           (Compound (pos src $1-start-pos $4-end-pos) (NameTerm-symbol $1) (flatten-commas $3))]

          ; Dictionary literals
          [(OPEN-BRACE KeyValuePairs CLOSE-BRACE) 
           (HashTable (pos src $1-start-pos $3-end-pos) (make-hash $2))]
          
          ; Vectors
          [(OPEN-VECTOR Term CLOSE-PAREN) 
           (Vector (pos src $1-start-pos $3-end-pos) (flatten-commas $2))]
          
          ; lists
          [(OPEN-BRACKET CLOSE-BRACKET) 
           (List (pos src $1-start-pos $2-end-pos) null #f)]
          [(OPEN-BRACKET Term BAR Term CLOSE-BRACKET) 
           (List (pos src $1-start-pos $5-end-pos) (flatten-commas $2) $4)]
          [(OPEN-BRACKET Term CLOSE-BRACKET) 
           (List (pos src $1-start-pos $3-end-pos) (flatten-commas $2) #f)]
          
          [(Term ASSIGN Term) (Compound (pos src $1-start-pos $3-end-pos) 'ASSIGN (list $1 $3))]
          
          ; numeric functions that could be evaluated are left as-is
          [(Term MINUS Term) (Compound (pos src $1-start-pos $3-end-pos) '- (list $1 $3))]
          [(Term PLUS  Term) (Compound (pos src $1-start-pos $3-end-pos) '+ (list $1 $3))]
          [(Term MULT  Term) (Compound (pos src $1-start-pos $3-end-pos) '* (list $1 $3))]
          [(Term DIV   Term) (Compound (pos src $1-start-pos $3-end-pos) '/  (list $1 $3))]
          
          [(Term RULE  Term) (Compound (pos src $1-start-pos $3-end-pos) 'RULE  (list $1 $3))]     
          [(Term DCG   Term) (Compound (pos src $1-start-pos $3-end-pos) 'DCG   (list $1 $3))]          
          [(Term IS    Term) (Compound (pos src $1-start-pos $3-end-pos) 'IS    (list $1 $3))]

          [(Term COMMA Term) (Compound (pos src $1-start-pos $3-end-pos) 'COMMA (list $1 $3))]
          
          [(Term PERIOD Term) (Accessor (pos src $1-start-pos $3-end-pos) $1 $3)]
          
          [(NOT-PROVABLE Term) (Compound (pos src $1-start-pos $2-end-pos) 'NOT-PROV (list $2))]
          
          [(Term UNIFY     Term) (Compound (pos src $1-start-pos $3-end-pos) 'UNIFY     (list $1 $3))]
          [(Term NOT-UNIFY Term) (Compound (pos src $1-start-pos $3-end-pos) 'NOT-UNIFY (list $1 $3))]
          [(Term IDENTICAL Term) (Compound (pos src $1-start-pos $3-end-pos) 'EQUAL     (list $1 $3))]
          [(Term NOT-IDENT Term) (Compound (pos src $1-start-pos $3-end-pos) 'NOT-EQUAL (list $1 $3))]

          [(Term NUM-LT Term) (Compound (pos src $1-start-pos $3-end-pos) 'LT (list $1 $3))]
          [(Term NUM-LE Term) (Compound (pos src $1-start-pos $3-end-pos) 'LE (list $1 $3))]
          [(Term NUM-GT Term) (Compound (pos src $1-start-pos $3-end-pos) 'GT (list $1 $3))]
          [(Term NUM-GE Term) (Compound (pos src $1-start-pos $3-end-pos) 'GE (list $1 $3))]
          [(Term NUM-NE Term) (Compound (pos src $1-start-pos $3-end-pos) 'NE (list $1 $3))]
          [(Term NUM-EQ Term) (Compound (pos src $1-start-pos $3-end-pos) 'EQ (list $1 $3))]

          [(Term SEMICOLON Term) (Compound (pos src $1-start-pos $3-end-pos) 'SEMI    (list $1 $3))]
          [(Term IF-THEN   Term) (Compound (pos src $1-start-pos $3-end-pos) 'IF-THEN (list $1 $3))]
          
          [(OPEN-PAREN Term CLOSE-PAREN) (Parens (pos src $1-start-pos $3-end-pos) $2)]
          [(OPEN-BRACE Term CLOSE-BRACE) (Compound (pos src $1-start-pos $3-end-pos) 'BRACES (list $2))]
                    
          [(Atom) $1]
          [(NUMBER)   (Number   (pos src $1-start-pos $1-end-pos) $1)]
          [(VARIABLE) (if (equal? $1 "_")
                          (AnonVariable (pos src $1-start-pos $1-end-pos) (string->symbol $1))
                          (Variable (pos src $1-start-pos $1-end-pos) (string->symbol $1)))]
          [(STRING)   (String   (pos src $1-start-pos $1-end-pos) $1)])
    
    (Atom [(ATOM) (Atom (pos src $1-start-pos $1-end-pos) (string->symbol $1))])
          
    )))

(define (flatten-commas term)
  (match term
    [(Compound _ 'COMMA `(,left ,right)) (cons left (flatten-commas right))]
    [_ (list term)]))

(define (pos src s-pos e-pos)
  (let* ([offset (position-offset s-pos)]
         [span (- (position-offset e-pos) offset)])  
    (vector src
          (position-line s-pos)
          (position-col s-pos)
          offset
          span)))