#lang racket
;; ------------------------------------------------------------------
;; Copyright (C) 2013, David Nick Main.
;;
;; See LICENSE file for details of license terms.
;; ------------------------------------------------------------------

;; A Model of Prolog code

(require "../parser/ast.rkt")
(provide (all-defined-out))

;; Base model node
(struct Meta ([metadata #:mutable]))  ; metadata is an optional hashtable

(struct PrologModule Meta (name         ; may be #f or Atom
                           exports      ; list of PredicateSpec - null for "all"
                           imports      ; list of ModuleImport
                           constraints  ; list of PredicateSpec
                           rules        ; list of ConstraintRule
                           predicates   ; list of Predicate
                           ) #:transparent)

(struct Predicate Meta (spec                ; PredicateSpec
                        [clauses #:mutable] ; list of Clause
                        ) #:transparent)

(struct PredicateSpec Meta (name  ; Atom
                            arity ; integer
                            ) #:transparent)

(struct ModuleImport Meta (module-ref
                           items   ; null for all, list of PredicateSpec 
                                   ; or ImportRename or ImportExclude
                           ) #:transparent)

(struct ModuleRef  Meta (name) #:transparent) ; name is Atom or String                             
(struct LibraryRef ModuleRef () #:transparent)

;; Import extensions to PredicateSpec
(struct ImportRename  PredicateSpec (rename) #:transparent) ; rename is PredicateSpec
(struct ImportExclude PredicateSpec () #:transparent)

(struct Clause Meta (head) #:transparent)  ; head is initially a NameTerm
(struct Fact   Clause    ()     #:transparent)
(struct Rule   Clause    (body-goal) #:transparent) ; body is a Goal

(struct Goal Meta () #:transparent)
(struct BinaryGoal Goal (left right) #:transparent)

(struct PredicateGoal Goal (spec  ; PredicateSpec
                            args  ; length = arity
                            ) #:transparent)

;; Built-in goals
(struct Conjunction BinaryGoal () #:transparent)
(struct Disjunction BinaryGoal () #:transparent)
(struct IfThen      Goal (condition then) #:transparent)
(struct IfThenElse  Goal (condition then else) #:transparent)
(struct NotProvable Goal (goal) #:transparent)
(struct Cut         Goal () #:transparent)
(struct True        Goal () #:transparent)
(struct Fail        Goal () #:transparent)
(struct Repeat      Goal () #:transparent)

(struct Is       BinaryGoal () #:transparent)
(struct Unify    BinaryGoal () #:transparent)
(struct NotUnify BinaryGoal () #:transparent)
(struct Equal    BinaryGoal () #:transparent)
(struct NotEqual BinaryGoal () #:transparent)

(struct Assign BinaryGoal () #:transparent)

(struct NumericBinaryGoal BinaryGoal () #:transparent)
(struct Numeric-<  NumericBinaryGoal () #:transparent)
(struct Numeric-<= NumericBinaryGoal () #:transparent)
(struct Numeric-!= NumericBinaryGoal () #:transparent)
(struct Numeric-=  NumericBinaryGoal () #:transparent)
(struct Numeric->  NumericBinaryGoal () #:transparent)
(struct Numeric->= NumericBinaryGoal () #:transparent)

(struct FindAll Goal (template goal result) #:transparent)

;; TODO
(struct ConstraintRule Meta (keeps 
                             drops guards body) #:transparent)
  
;; Recursively gather names of variables in the Term or Goal
;; into the given hashtable. Ignore anonymous variables.
(define (gather-vars goal-or-term var-hash)
  (define (recurse a) (gather-vars a var-hash))
  (match goal-or-term
    [(AnonVariable _ _)    (void)]
    [(Variable _ symbol)   (hash-set! var-hash symbol #t)]
    [(Compound _ _ args)   (map recurse args)]
    [(Parens _ term)       (recurse term)]
    [(HashTable _ hashtbl) (map recurse (hash-values hashtbl))]
    [(List _ head tail)    (map recurse head) (recurse tail)]
    [(Vector _ elements)   (map recurse elements)]
    [(Accessor _ t k)      (recurse t) (recurse k)]
    [(BinaryGoal _ l r)    (recurse l) (recurse r)]
    [(IfThen _ c t)        (recurse c) (recurse t)]
    [(IfThenElse _ c t e)  (recurse c) (recurse t) (recurse e)]
    [(NotProvable _ goal)  (recurse goal)]
    [(PredicateGoal _ _ a) (map recurse a)]
    [(FindAll _ t g r)     (recurse t) (recurse g) (recurse r)]
    [_ (void)]))

;; Get metadata value from a node - return default if no such key
(define (get-metadata model-node key default)
  (match model-node
    [(Meta md) (if (and (hash? md)
                        (hash-has-key? md key))
                   (hash-ref md key default)
                   default)]
    [_ default]))

;; Set a metadata property
(define (set-metadata! model-node key value)
  (match model-node
    [(Meta md) (if (hash? md)
                   (hash-set! md key value)
                   (set-Meta-metadata! 
                    (make-hash (cons key value))))]
    [_ (void)]))
