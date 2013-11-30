#lang racket
;; ------------------------------------------------------------------
;; Copyright (C) 2013, David Nick Main.
;;
;; See LICENSE file for details of license terms.
;; ------------------------------------------------------------------

(require parser-tools/lex)

(provide (all-defined-out))

;; Returns a thunk that calls the filter-proc for each token 
;; (content of the lexeme) and drops if the result is #f
(define (lexeme-filter lexer-thunk filter-proc)
  (thunk
   (let next-lexeme ([lexeme (lexer-thunk)])
     (if (filter-proc (position-token-token lexeme))
         lexeme
         (next-lexeme (lexer-thunk))))))

;; Returns a thunk that drops 'whitespace tokens
(define (whitespace-filter lexer-thunk)
  (lexeme-filter lexer-thunk (λ(token)(not (eq? token 'whitespace)))))

;; Returns (values peek read), where
;;  peek is a thunk that peeks at the next lexeme, and
;;  read is a thunk that reads (consumes) the next lexeme
(define (lexeme-peeker lexer-thunk)
  (let ([memo #f])
    (values
     (thunk (unless memo (set! memo (lexer-thunk)))
            memo)
     (thunk (if memo
                (begin0 memo
                        (set! memo #f))
                (lexer-thunk))))))

;; Returns a thunk that calls the map-proc for each lexeme
;; and returns the result.
;; The map-proc is passed the current lexeme and the next.
(define (lookahead-lexeme-map lexer-thunk map-proc)
  (let-values ([(peeker reader) (lexeme-peeker lexer-thunk)])
    (thunk
     (let ([this-lex (reader)]
           [next-lex (peeker)])
       (map-proc this-lex next-lex)))))
  
;; convert PERIOD+(whitespace/EOF) to TERM-END
(define (term-end-detector this-lex next-lex)
  (let ([this-token (position-token-token this-lex)]
        [next-token (position-token-token next-lex)])
    (if (and (eq? this-token 'PERIOD)
             (or (eq? next-token 'whitespace)
                 (eq? next-token 'EOF)))
        (make-position-token 'TERM-END
                             (position-token-start-pos this-lex)
                             (position-token-end-pos   this-lex))
        this-lex)))