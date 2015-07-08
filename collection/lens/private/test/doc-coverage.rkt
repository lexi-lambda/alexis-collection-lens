#lang racket/base

(module+ test
  (require doc-coverage
         alexis/collection/lens)
  (check-all-documented 'alexis/collection/lens))
