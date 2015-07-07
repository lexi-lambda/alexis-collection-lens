#lang info

(define name "alexis-collection-lens")
(define version "0.1.0")

(define collection "alexis")

(define scribblings '(("scribblings/main.scrbl" () (library) "alexis-collection-lens")))

(define deps
  '("base"
    "alexis-collections"
    "lens"
    "scribble-lib"))
(define build-deps
  '("at-exp-lib"
    "curly-fn"
    "rackunit-lib"
    "racket-doc"))
