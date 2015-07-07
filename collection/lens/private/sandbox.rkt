#lang racket/base

(require scribble/eval)

(provide lens:interaction
         lens:examples)

(define lens:evaluator
  (make-eval-factory
   #:lang 'racket
   '(alexis/collection
     lens
     alexis/collection/lens)))

(define-syntax-rule (lens:interaction . body)
  (interaction #:eval (lens:evaluator) . body))

(define-syntax-rule (lens:examples . body)
  (examples #:eval (lens:evaluator) . body))
