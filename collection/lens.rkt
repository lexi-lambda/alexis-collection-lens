#lang curly-fn at-exp racket/base

(require alexis/collection
         lens
         scribble/srcdoc
         racket/contract
         (for-doc racket/base
                  scribble/manual
                  "lens/private/sandbox.rkt"))

(provide @proc-doc/names[ref-lens (any/c . -> . lens?) (key)
                         @{Constructs a lens that operates on @racket[indexable?] instances.
                           @(lens:examples
                             (lens-transform (ref-lens 'key)
                                             (λ (x) (* 10 x))
                                             (hash 'key 2)))}]
         @proc-doc/names[nth-lens (exact-nonnegative-integer? . -> . lens?) (index)
                         @{Constructs a lens that operates on @racket[sequence?] instances.
                           @(lens:examples
                             (lens-transform (nth-lens 1)
                                             (λ (x) (* 10 x))
                                             '(1 2 3)))}])

(define (ref-lens key)
  (make-lens (λ (target) (ref target key))
             (λ (target value) (set-ref target key value))))

(define (nth-lens index)
  (make-lens (λ (target) (nth target index))
             (λ (target value) (set-nth target index value))))

(module+ test
  (require rackunit)
  (test-case
   "ref-lens"
   (check-equal? (lens-transform (ref-lens 'key) #{* 10} (hash 'key 2))
                 (hash 'key 20)))
  (test-case
   "nth-lens"
   (check-equal? (lens-transform (nth-lens 1) #{* 10} '(1 2 3))
                 '(1 20 3))))
