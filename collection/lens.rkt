#lang curly-fn at-exp racket/base

(require alexis/collection
         (only-in lens lens? make-lens lens-view lens-set lens-thrush)
         scribble/srcdoc
         racket/contract
         (for-doc racket/base
                  scribble/manual
                  "lens/private/sandbox.rkt"))

(provide @proc-doc/names[ref-lens
                         (case-> (any/c . -> . lens?)
                                 (any/c any/c . -> . lens?))
                         ((key) (key default))
                         @{Constructs a lens for a keyed value of an @racket[indexable?] instance.
                           @(lens:examples
                             (lens-transform (ref-lens 'key)
                                             (hash 'key 2)
                                             (λ (x) (* 10 x)))
                             (lens-view (ref-lens 'key 'default)
                                        (hash)))}]
         @proc-doc/names[nth-lens (exact-nonnegative-integer? . -> . lens?) (index)
                         @{Constructs a lens for a single element of a @racket[sequence?].
                           @(lens:examples
                             (lens-transform (nth-lens 1)
                                             '(1 2 3)
                                             (λ (x) (* 10 x))))}]
         @proc-doc/names[map-lens (lens? . -> . lens?) (lens)
                         @{Constructs a lens that maps over @racket[sequence?] instances.
                           @(lens:examples
                             (lens-view (map-lens (ref-lens 'key))
                                        (list (hash 'key 1 'a 2) (hash 'key 2 'b 3)))
                             (sequence->list
                              (lens-view (map-lens (ref-lens 'key))
                                         (list (hash 'key 1 'a 2) (hash 'key 2 'b 3))))
                             (sequence->list
                              (lens-set (map-lens (ref-lens 'key))
                                        (list (hash 'key 1 'a 2) (hash 'key 2 'b 3))
                                        (list 100 200)))
                             (sequence->list
                              (lens-transform (map-lens (ref-lens 'key))
                                              (list (hash 'key 1 'a 2) (hash 'key 2 'b 3))
                                              (λ (lst) (map (λ (x) (* 10 x)) lst)))))}]
         @thing-doc[first-lens lens?
                    @{A lens for the first element of a @racket[sequence?].}]
         @thing-doc[rest-lens lens?
                    @{A lens for all but the first element of a @racket[sequence?].}]
         @proc-doc/names[take-lens (exact-nonnegative-integer? . -> . lens?) (n)
                         @{Constructs a lens for the first @racket[n] elements of a
                           @racket[sequence?].}]
         @proc-doc/names[drop-lens (exact-nonnegative-integer? . -> . lens?) (n)
                         @{Constructs a lens for the rest of a @racket[sequence?] after the first
                           @racket[n] elements.}]
         @proc-doc/names[subsequence-lens
                         (exact-nonnegative-integer? exact-nonnegative-integer? . -> . lens?)
                         (start end)
                         @{A lens for a subsequence from @racket[start] to @racket[end]. The range
                           behavior corresponds to @racket[subsequence].}]
         @proc-doc/names[subsequence*-lens
                         (exact-nonnegative-integer? exact-nonnegative-integer? . -> . lens?)
                         (start len)
                         @{A lens for a subsequence starting at @racket[start] with length
                           @racket[len] elements. The range behavior corresponds to
                           @racket[subsequence*].}])

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

(define (map-lens lens)
  (make-lens (λ (target) (map #{lens-view lens %1} target))
             (λ (target value) (map #{lens-set lens %1 %2} target value))))

(define (take-lens n)
  (make-lens (λ (target) (take n target))
             (λ (target value) (append value (drop n target)))))

(define (drop-lens n)
  (make-lens (λ (target) (drop n target))
             (λ (target value) (append (take n target) value))))

(define first-lens (nth-lens 0))
(define rest-lens (drop-lens 1))

(define (subsequence*-lens start len)
  (lens-thrush (drop-lens start)
               (take-lens len)))

(define (subsequence-lens start end)
  (subsequence*-lens start (- end start)))

(module+ test
  (require rackunit (only-in lens lens-transform))
  (test-case
   "ref-lens"
   (check-equal? (lens-transform (ref-lens 'key) (hash 'key 2) #{* 10})
                 (hash 'key 20))
   (check-equal? (lens-view (ref-lens 'key 'default) (hash))
                 'default))
  (test-case
   "nth-lens"
   (check-equal? (lens-transform (nth-lens 1) '(1 2 3) #{* 10})
                 '(1 20 3)))
  (test-case
   "map-lens"
   (check-equal? (sequence->list
                  (lens-transform (map-lens (ref-lens 'key))
                                  (list (hash 'key 1) (hash 'key 2))
                                  (λ (lst) (map #{* 10} lst))))
                 (list (hash 'key 10) (hash 'key 20)))))
