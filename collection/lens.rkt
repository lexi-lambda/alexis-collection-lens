#lang curly-fn at-exp racket/base

(require alexis/collection
         lens
         scribble/srcdoc
         racket/contract
         (for-doc racket/base
                  scribble/manual
                  "lens/private/sandbox.rkt"))

(provide @proc-doc/names[ref-lens
                         (case-> (any/c . -> . lens?)
                                 (any/c any/c . -> . lens?))
                         ((key) (key default))
                         @{Constructs a lens that operates on @racket[indexable?] instances.
                           @(lens:examples
                             (lens-transform (ref-lens 'key)
                                             (hash 'key 2)
                                             (λ (x) (* 10 x)))
                             (lens-view (ref-lens 'key 'default)
                                        (hash)))}]
         @proc-doc/names[nth-lens (exact-nonnegative-integer? . -> . lens?) (index)
                         @{Constructs a lens that operates on @racket[sequence?] instances.
                           @(lens:examples
                             (lens-transform (nth-lens 1)
                                             '(1 2 3)
                                             (λ (x) (* 10 x))))}])

(define ref-lens
  (case-lambda
    [(key)
     (make-lens (λ (target) (ref target key))
                (λ (target value) (set-ref target key value)))]
    [(key default)
     (make-lens (λ (target) (ref target key default))
                (λ (target value) (set-ref target key value)))]))

(define (nth-lens index)
  (make-lens (λ (target) (nth target index))
             (λ (target value) (set-nth target index value))))

(module+ test
  (require rackunit)
  (test-case
   "ref-lens"
   (check-equal? (lens-transform (ref-lens 'key) (hash 'key 2) #{* 10})
                 (hash 'key 20))
   (check-equal? (lens-view (ref-lens 'key 'default) (hash))
                 'default))
  (test-case
   "nth-lens"
   (check-equal? (lens-transform (nth-lens 1) '(1 2 3) #{* 10})
                 '(1 20 3))))
