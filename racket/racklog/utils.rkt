#lang racket
;; ------------------------------------------------------------------
;; Copyright (C) 2013, David Nick Main.
;;
;; See LICENSE file for details of license terms.
;; ------------------------------------------------------------------

;; Some common predicates for Racklog

(require racklog)
(provide (all-defined-out))

(define get_time/1
  (%rel (A)
        [(A) (%is A (/ (current-inexact-milliseconds) 1000))]))
  
(define printf/2
  (%rel (Fmt Args)
        [(Fmt Args) (%is (_) (apply printf Fmt Args))])) 

(define length/2
  (%rel (List Length)
        [(List Length) (%is Length (length List))]))