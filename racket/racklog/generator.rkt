#lang racket
;; ------------------------------------------------------------------
;; Copyright (C) 2013, David Nick Main.
;;
;; See LICENSE file for details of license terms.
;; ------------------------------------------------------------------

;; Code generator for Racklog

(require syntax/readerr)
(require "../parser/ast.rkt")
(require "../model/ast-to-model.rkt")
(require "../model/model.rkt")

(provide model->racklog)

;; Generate Racklog definitions from a Prolog model
(define (model->racklog model)
  (map model->relation (PrologModule-predicates model)))

(define (model->relation predicate)
  (let* ([var-names (make-hash)]
         [spec (Predicate-spec predicate)]
         [name (predicate-key spec)]
         [name-stx (datum->syntax #f 
                                  (string->symbol name)
                                  (AST-position (PredicateSpec-name spec)))]
         [clauses (map (λ(c)(model->clause c var-names)) 
                       (Predicate-clauses predicate))])
    
    ; Racklog relation definition:
    `(define ,name-stx (%rel ,(hash-keys var-names) ,@clauses))))
    
(define (model->clause clause var-hash)  
  (let* ([head (Clause-head clause)]
         [head-pattern (match head
                         [(Atom _ _) '()]
                         [(Compound _ _ args) `(,@(map ast->form args))])]
         [body (if (Rule? clause)
                   (let ([goal (Rule-body-goal clause)])
                     (gather-vars goal var-hash)                 
                     (goal->model (Rule-body-goal clause)))
                   '%true)])
    (gather-vars head var-hash)
    
    ; racklog clause
    `[,head-pattern ,body] ))

;; Generate scheme for rule body (recursive for conjunction and disjunction)
(define (goal->model goal)
  (if goal
      (match goal                
        [(PredicateGoal _ spec args) 
         `(,(datum->syntax #f 
                           (string->symbol (predicate-key spec))
                           (AST-position (PredicateSpec-name spec)))
           ,@(map ast->form args))]
        
        [(IfThen _ con then) `(%if-then-else ,(goal->model con)
                                             ,(goal->model then)
                                             %fail)]

        [(IfThenElse _ con then else) `(%if-then-else ,(goal->model con)
                                                     ,(goal->model then)
                                                     ,(goal->model else))]

        [(Conjunction _ left right) `(%and ,(goal->model left)
                                           ,(goal->model right))]

        [(Disjunction _ left right) `(%or ,(goal->model left)
                                          ,(goal->model right))]

        [(Is _ left right) `(%is ,(ast->form left) ,(ast->eval-form right))]
        
        [(Unify    _ left right) `(%=   ,(ast->form left) ,(ast->form right))]
        [(NotUnify _ left right) `(%/=  ,(ast->form left) ,(ast->form right))]
        [(Equal    _ left right) `(%==  ,(ast->form left) ,(ast->form right))]
        [(NotEqual _ left right) `(%/== ,(ast->form left) ,(ast->form right))]
        
        [(NotProvable _ goal) `(%not ,(goal->model goal))] 
        
        [(Cut    _) '!]
        [(True   _) '%true]
        [(Fail   _) '%fail]
        [(Repeat _) '(%repeat)]
        
        [(Numeric-<  _ left right) (eval-form '%<   left right)] 
        [(Numeric-<= _ left right) (eval-form '%<=  left right)] 
        [(Numeric->  _ left right) (eval-form '%>   left right)] 
        [(Numeric->= _ left right) (eval-form '%>=  left right)] 
        [(Numeric-=  _ left right) (eval-form '%=:= left right)] 
        [(Numeric-!= _ left right) (eval-form '%=/= left right)] 
        
        [(FindAll _ template goal result)
         `(%bag-of ,(ast->form template) ,(goal->model goal) ,(ast->form result) )])
                
      ; no goal => simple success
      '%true))

;; Make a goal that evaluates its arguments
(define (eval-form pred left right)
  `(%let (Left Right)
         (%and
          (%and (%is Left  ,(ast->eval-form left))
                (%is Right ,(ast->eval-form right)))
         (,pred Left Right))))

;; Convert AST term to scheme form
(define (ast->form ast)
  (match ast
    [(Parens   _ term)  (ast->form term)]
    [(Number   p value) (datum->syntax #f value p)]
    [(Atom     p symb)  (datum->syntax #f `(quote ,symb) p)]
    [(AnonVariable p _) (datum->syntax #f '(_) p)]
    [(Variable p name)  (datum->syntax #f name p)]
    [(String   p text)  (datum->syntax #f text p)]
    [(Vector   p elems) (datum->syntax #f `(vector ,@(map ast->form elems)) p)]    
    
    [(Compound p functor args) (let ([arg-forms (map ast->form args)])
                                 (datum->syntax #f `(list (quote ,functor)
                                                          ,@arg-forms) p))]
    
    [(HashTable p hashtable) (let ([pair-forms 
                                    (hash-map 
                                     hashtable
                                     (λ(k v) `(cons ,(ast->form k)
                                                    ,(ast->form v))))])
                               (datum->syntax #f `(make-hash (list ,@pair-forms)) p))]
    
    [(List p head tail) (let ([head-forms (map ast->form head)])
                          (if tail
                              (datum->syntax #f `(list* ,@head-forms ,(ast->form tail)) p)
                              (datum->syntax #f `(list ,@head-forms) p)))]))


;; Convert AST term to scheme form that can be evaluated
(define (ast->eval-form ast)
  (match ast
    [(Parens   _ term)  (ast->eval-form term)]
    [(Number   p value) (datum->syntax #f value p)]
    [(Atom     p symb)  (datum->syntax #f `(quote ,symb) p)]
    [(Variable p symb)  (datum->syntax #f symb p)]
    [(String   p text)  (datum->syntax #f text p)]

    [(Vector p elems) (datum->syntax #f `(vector ,@(map ast->eval-form elems)) p)]
        
    [(Compound p functor args) (let ([arg-forms (map ast->eval-form args)])
                                          (datum->syntax #f `(,functor
                                                              ,@arg-forms) p))]

    [(HashTable p hashtable) (let ([pair-forms (hash-map 
                                                hashtable
                                                (λ(k v) `(cons ,(ast->form k)
                                                               ,(ast->form v))))])
                               (datum->syntax #f `(make-hash (list ,@pair-forms)) p))]
    
    [(List p head tail) (let ([head-forms (map ast->form head)])
                          (if tail
                              (datum->syntax #f `(list* ,@head-forms ,(ast->form tail)) p)
                              (datum->syntax #f `(list ,@head-forms) p)))]))