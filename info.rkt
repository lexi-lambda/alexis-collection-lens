#lang info

(define name "alexis-collection-lens")
(define version "0.1.0")

(define collection "alexis")

(define scribblings '(("scribblings/main.scrbl" () (library) "alexis-collection-lens")))

(define deps
  '("base"
    "alexis-collections"
    "lens"
    "curly-fn"
    "scribble-lib"))
(define build-deps
  '("at-exp-lib"
    "rackunit-lib"
    "racket-doc"
    "doc-coverage"))
