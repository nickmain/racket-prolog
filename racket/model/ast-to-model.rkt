#lang racket
;; ------------------------------------------------------------------
;; Copyright (C) 2013, David Nick Main.
;;
;; See LICENSE file for details of license terms.
;; ------------------------------------------------------------------

;; Build a Prolog model from an AST

(require "../parser/ast.rkt")
(require "../parser/definite-clause-grammar.rkt")
(require "model.rkt")
(require syntax/readerr)

(provide ast-list->model
         predicate-key
         barf)

(define (ast-list->model ast-list)
  (let ([module-name #f]
        [module-dir  #f]
        [rules       null]
        [predicates  (make-hash)]
        [constraints (make-hash)]
        [exports     (make-hash)]
        [imports     (make-hash)])
    
    (define (process-ast ast)
      (match ast
        [(Directive dp term)
         (match term
           [(Compound p 'module args) (when module-dir (barf p "More than one module directive"))
                                      (set! module-dir #t)
                                      (set! module-name (process-module p args exports))]
           
           [(Compound _ 'use_module args) (process-import args imports)]
           [_ (barf dp "Unrecognized directive")])]
        
        ; TODO - Constraint declarations
        ; TODO - Rules
        
        ; Definite Clause Grammar rules are rewritten to normal rules
        [(Compound _ 'DCG _) (process-ast (rewrite-dcg-rule ast))]
        
        [(Atom p symbol) (process-clause ast predicates)]
        [(Compound p _ _) (process-clause ast predicates)]
        
        [_ (barf (AST-position ast) "Unrecognized Prolog form.")]
        ))
    
    (for-each process-ast ast-list)
    
    (PrologModule #f
                  module-name
                  (hash-values exports)
                  (hash-values imports)
                  (hash-values constraints)
                  (reverse rules)
                  (hash-values predicates))))
                  

(define (process-clause term predicate-hash)
  (match term
    [(Atom p symbol) 
     (add-clause (PredicateSpec #f term 0) term #f predicate-hash)]
    
    [(Compound p 'RULE `(,head ,body)) 
     (match head
       [(Atom pa symbol) 
        (add-clause (PredicateSpec #f head 0) 
                    head 
                    body 
                    predicate-hash)]
       
       [(Compound p functor args) 
        (add-clause (PredicateSpec #f (Atom p functor) (length args))
                    head 
                    body 
                    predicate-hash)]

       [_ (barf (AST-position head) "Bad rule head.")])]
    
    [(Compound p functor args) 
     (add-clause (PredicateSpec #f (Atom p functor) (length args))
                 term 
                 #f 
                 predicate-hash)]
    
    [_ (barf (AST-position term) "Unrecognized predicate form.")]))

(define (add-clause pred-spec head body predicate-hash)
  (let* ([pred-key (predicate-key pred-spec)]
         [predicate (hash-ref! predicate-hash
                               pred-key
                               (thunk (Predicate #f pred-spec null)))])
    (append-clause predicate
                   (if body
                       (Rule #f head (body->goal body))
                       (Fact #f head)))))

(define (append-clause predicate clause)
  (set-Predicate-clauses! 
   predicate
   (append (Predicate-clauses predicate)
           (list clause)))) 
  
(define (body->goal body)
  (match body
    [(Parens _ term) (body->goal term)]
    
    [(Compound _ 'COMMA   `(,left ,right)) (Conjunction #f (body->goal left) (body->goal right))]
    [(Compound _ 'IF-THEN `(,left ,right)) (IfThen      #f (body->goal left) (body->goal right))]    
    [(Compound _ 'SEMI  `(,left ,right)) 
     (match left 
       [(Compound _ 'IF-THEN `(,cnd ,thn)) ; cond -> then ; else
        (IfThenElse #f (body->goal cnd) (body->goal thn) (body->goal right))]       
       [_ (Disjunction #f (body->goal left) (body->goal right))])]
    
    [(Compound _ 'IS        `(,left ,right)) (Is       #f left right)]
    [(Compound _ 'UNIFY     `(,left ,right)) (Unify    #f left right)]
    [(Compound _ 'NOT-UNIFY `(,left ,right)) (NotUnify #f left right)]
    [(Compound _ 'EQUAL     `(,left ,right)) (Equal    #f left right)]
    [(Compound _ 'NOT-EQUAL `(,left ,right)) (NotEqual #f left right)]
    
    [(Compound _ 'ASSIGN `(,target ,key)) (Assign #f target key)]
    
    [(Compound _ 'LT `(,left ,right)) (Numeric-<  #f left right)]
    [(Compound _ 'LE `(,left ,right)) (Numeric-<= #f left right)]
    [(Compound _ 'NE `(,left ,right)) (Numeric-!= #f left right)]
    [(Compound _ 'EQ `(,left ,right)) (Numeric-=  #f left right)]
    [(Compound _ 'GT `(,left ,right)) (Numeric->  #f left right)]
    [(Compound _ 'GE `(,left ,right)) (Numeric->= #f left right)]
    
    [(Compound _ 'findall `(,templ ,goal ,result)) (FindAll #f templ (body->goal goal) result)]
    
    [(Compound _ 'NOT-PROV `(,goal)) (NotProvable #f (body->goal goal))]
    
    [(Compound p functor args) 
     (PredicateGoal #f (PredicateSpec #f (Atom p functor) (length args)) args)]
    
    [(Atom _ '!)      (Cut #f)]
    [(Atom _ 'true)   (True #f)]
    [(Atom _ 'fail)   (Fail #f)]
    [(Atom _ 'repeat) (Repeat #f)]
    
    [(Atom _ _) (PredicateGoal #f (PredicateSpec #f body 0) null)]
    
    [_ (barf (AST-position body) "Bad goal term")]))
  
;; TODO - handle import items
;; TODO - handle library imports
(define (process-import import-args import-hash)
  (match import-args
    [(list (String p text)) (if (hash-has-key? import-hash text)
                                (barf p "Duplicate import.")
                                (hash-set! import-hash 
                                           text 
                                           (ModuleImport #f 
                                                         (ModuleRef #f (String p text)) 
                                                         null)))]
    [(list (Atom p name)) (if (hash-has-key? import-hash name)
                              (barf p "Duplicate import.")
                              (hash-set! import-hash 
                                         name 
                                         (ModuleImport #f 
                                                       (ModuleRef #f (Atom p name)) 
                                                       null)))]
    [_ (barf #f (~a "Bad use_module directive: " import-args))]))
    

(define (process-module posn mod-dir-args export-hash)
  (match mod-dir-args
    [(list (Atom p symbol) (List _ head #f)) (add-exports head export-hash)
                                             (Atom p symbol)]
    [(list (Variable _ _ ) (List _ head #f)) (add-exports head export-hash)
                                             #f]
    [_ (barf posn "Bad module directive")]))

;; add exports to the hash
(define (add-exports export-list export-hash)
  (map (λ(export-term)
         (let* ([export-spec (predicate-spec export-term)]
                [export-key  (predicate-key export-spec)])
           (if (hash-has-key? export-hash export-key)
               (barf (AST-position export-term) "Duplicate export.")
               (hash-set! export-hash export-key export-spec))))           
       export-list))

;; make a string key for a predicate spec
(define (predicate-key pred-spec)
  (match pred-spec
    [(PredicateSpec _ (Atom _ name) arity) (~a name "/" arity)]
    [_ (barf #f (~a "Bad predicate-spec: " pred-spec))]))

;; make a predicate spec from a term
(define (predicate-spec term)
  (match term
    [(Compound _ '/ (list (Atom p name) 
                          (Number n arity)))
     (if (and (integer? arity)
              (>= arity 0))
         (PredicateSpec #f (Atom p name) arity)
         (barf n "Predicate arity must be integer >= 0"))]
    
    [(AST p) (barf p "Bad predicate spec")]))
  
;; Raise read error
(define (barf posn msg)  
  (if posn
      (raise-read-error msg
                        (vector-ref posn 0)
                        (vector-ref posn 1)
                        (vector-ref posn 2)
                        (vector-ref posn 3)
                        (vector-ref posn 4))
      (raise-read-error msg 
                        #f #f #f #f #f)))
