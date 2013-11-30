#lang racket
;; ------------------------------------------------------------------
;; Copyright (C) 2013, David Nick Main.
;;
;; See LICENSE file for details of license terms.
;; ------------------------------------------------------------------

;; Prolog lexer

(require parser-tools/lex)
(require parser-tools/lex-sre)
(require syntax/readerr)
(require "lexer-utils.rkt")

(provide (all-defined-out))

(define-tokens value-tokens (NUMBER 
                             ATOM
                             STRING
                             VARIABLE))

(define-empty-tokens delimiter-tokens (TERM-END 
                                       EOF
                                       OPEN-VECTOR
                                       OPEN-PAREN CLOSE-PAREN
                                       OPEN-BRACE CLOSE-BRACE
                                       OPEN-BRACKET CLOSE-BRACKET))

(define-empty-tokens op-tokens (RULE DCG QUERY
                                AT-SIGN BACKSLASH EQ-EQ-ARROW EQ-DOUBLE-ARROW
                                COLON SEMICOLON BAR IF-THEN COMMA UNIFY
                                NOT-UNIFY IDENTICAL NOT-IDENT
                                NOT-PROVABLE
                                NUM-LT NUM-LE NUM-GT NUM-GE NUM-NE NUM-EQ
                                IS PLUS MINUS MULT DIV
                                PERIOD ASSIGN))

(define-lex-abbrevs
  (hex-digit  (char-set "0123456789abcdefABCDEF"))
  (hex-prefix (or "0x" "0X"))
  (numeric-sign (or #\- #\+))
  (digit10 (/ #\0 #\9))
  (digit9  (/ #\1 #\9))
  (number  (or #\0 (: digit9 (* digit10))))
  (decimal (: #\. (+ digit10))))

;; For testing - read all lexemes
(define (prolog-lex-all src in)
  (let [(lex-thunk (prolog-lexer src in))]
    (sequence->list 
     (in-producer 
      lex-thunk 
      (λ(lex)(eq? (position-token-token lex) 'EOF))))))

(define (prolog-lexer src in)
  (let* ([lex-thunk  (thunk 
                      (file-path src)
                      (the-lexer in))]
         [term-ender (lookahead-lexeme-map lex-thunk term-end-detector)]
         [no-whitespace (whitespace-filter term-ender)])
    no-whitespace))

(define the-lexer   
  (lexer-src-pos
   [(eof) (token-EOF)]
   [(+ whitespace) 'whitespace]
   [#\% (begin (read-line input-port 'any) 'whitespace)]
   
   ["#(" (token-OPEN-VECTOR)]
   
   [#\( (token-OPEN-PAREN)]   [#\) (token-CLOSE-PAREN)]
   [#\{ (token-OPEN-BRACE)]   [#\} (token-CLOSE-BRACE)]
   [#\[ (token-OPEN-BRACKET)] [#\] (token-CLOSE-BRACKET)]
   
   [#\! (token-ATOM "!")]
   ["is" (token-IS)]
   
   [(: lower-case (* (or alphabetic numeric #\_))) (token-ATOM lexeme)]
   [(: #\' (+ (or "''" (~ #\'))) #\') (token-ATOM (unquote-atom lexeme))]   
   [(: #\" (+ (or "\\\"" (~ #\"))) #\") (token-STRING (unquote-string lexeme))]   
   [(: (or upper-case #\_) (* (or alphabetic numeric #\_))) (token-VARIABLE lexeme)]

   [(: hex-prefix (+ hex-digit)) (token-NUMBER (string->number (substring lexeme 2) 16))]   
   [(: (? numeric-sign) number (? decimal)) (token-NUMBER (string->number lexeme))]

   [":-" (token-RULE)]
   ["-->" (token-DCG)]
   ["?-" (token-QUERY)]
   [":" (token-COLON)]
   [";" (token-SEMICOLON)]
   ["|" (token-BAR)]
   ["->" (token-IF-THEN)]
   ["," (token-COMMA)]
   ["+" (token-PLUS)] 
   ["-" (token-MINUS)]
   ["*" (token-MULT)]
   ["/" (token-DIV)]
   ["." (token-PERIOD)]
   ["<-" (token-ASSIGN)]
   ["\\+" (token-NOT-PROVABLE)]

   ["=<"   (token-NUM-LE)]
   ["<"    (token-NUM-LT)]
   ["=\\=" (token-NUM-NE)]
   ["=:="  (token-NUM-EQ)]
   [">="   (token-NUM-GE)]
   [">"    (token-NUM-GT)]
   
   ["\\="  (token-NOT-UNIFY)]
   ["=="   (token-IDENTICAL)]
   ["\\==" (token-NOT-IDENT)]
   ["="    (token-UNIFY)]

   ["@"   (token-AT-SIGN)]   
   ["\\"  (token-BACKSLASH)]   
   ["<=>" (token-EQ-DOUBLE-ARROW)]   
   ["==>" (token-EQ-EQ-ARROW)]      
   ))   

;; TODO: other escape sequences
(define (unquote-atom s)
  (string-replace
   (string-replace (string-trim s "'") "''" "'")
   "\\n" "\n"))

;; TODO: other escape sequences
(define (unquote-string s)
  (string-replace
   (string-replace (string-trim s "\"") "\\\"" "\"")
   "\\n" "\n"))