#lang racket
;; ------------------------------------------------------------------
;; Copyright (C) 2013, David Nick Main.
;;
;; See LICENSE file for details of license terms.
;; ------------------------------------------------------------------

(require "../parser/lexer.rkt")
(require "../parser/parser.rkt")
(require "../parser/ast.rkt")
(require "../model/model.rkt")
(require "../model/ast-to-model.rkt")
(require "generator.rkt")

;;====================================================================
;; Reader that parses Prolog source and generates the equivalent
;; Racklog code.
;;====================================================================

(provide read read-syntax)
 
(define (read in) 
  ; convert datum/syntax mixture to syntax and then back to datum
  (syntax->datum (datum->syntax #f (read-syntax #f in))))

(define (read-syntax src in)
  (port-count-lines! in)
  (let* ([lexer    (prolog-lexer src in)]
         [parser   (prolog-parser src)]
         [ast-list (parser lexer)]
         [model    (ast-list->model ast-list)]
         [top-level-forms (model->racklog model)]
         
         [module-name (match (PrologModule-name model)
                        [(Atom p name) (datum->syntax #f name p)]
                        [_ (string->symbol (~a src))])]
         
         [exports (PrologModule-exports model)]         
         [provides (if (null? exports)
                       '(provide (all-defined-out))
                       `(provide ,@(map string->symbol 
                                        (map predicate-key exports))))]
         
         [requires (map (λ(imp)
                          (match imp
                            [(ModuleImport _ (ModuleRef _ name) _)
                             `(require ,(match name
                                          [(Atom p symbol) (datum->syntax #f (~a symbol ".rkt") p)]
                                          [(String p text) (datum->syntax #f text p)]
                                          [_ (barf #f (~a "Bad module name: " name))]))]
                            [_ (barf #f (~a "Bad module import: " imp))]))
                            
                        (PrologModule-imports model))])
    
    `(module ,module-name racket
       (require racklog)       
       ,@requires
       ,provides
       
       ,@top-level-forms 
       
       #| DEBUG:
       ,@(map (λ(f) `(printf "~a\n" (quote ,f))) requires)
       (printf "~a\n" (quote ,provides))
       ,@(map (λ(f) `(printf "~a\n" (quote ,f))) top-level-forms) ;|#
       )))