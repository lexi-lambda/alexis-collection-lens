#lang info

(define name "alexis-collection-lens")
(define version "0.2.0")

(define collection "alexis")

(define scribblings '(("scribblings/main.scrbl" () (library) "alexis-collection-lens")))

(define deps
  '("alexis-collections"
    "base"
    "curly-fn"
    "lens"
    "scribble-lib"))
(define build-deps
  '("at-exp-lib"
    "cover"
    "cover-coveralls"
    "doc-coverage"
    "rackunit-lib"
    "racket-doc"))
