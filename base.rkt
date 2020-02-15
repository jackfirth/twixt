#lang racket/base

(require racket/contract/base)

(provide twixt-peg
         twixt-position)

(provide
 (contract-out
  [standard-twixt-board-size natural?]
  [standard-twixt-border-length natural?]
  [twixt-index/c flat-contract?]
  [twixt-position? predicate/c]
  [twixt-position-row (-> twixt-position? twixt-index/c)]
  [twixt-position-column (-> twixt-position? twixt-index/c)]
  [twixt-player? predicate/c]
  [red twixt-player?]
  [black twixt-player?]
  [twixt-peg? predicate/c]
  [twixt-peg-owner (-> twixt-peg? twixt-player?)]
  [twixt-peg-position (-> twixt-peg? twixt-position?)]
  [red-twixt-peg (-> #:row twixt-index/c #:column twixt-index/c twixt-peg?)]
  [black-twixt-peg (-> #:row twixt-index/c #:column twixt-index/c twixt-peg?)]
  [twixt-board? predicate/c]
  [empty-twixt-board twixt-board?]
  [twixt-board-get-peg (-> twixt-board? twixt-position? (option/c twixt-peg?))]
  [twixt-board-occupied-at? (-> twixt-board? twixt-position? boolean?)]
  [twixt-board-unoccupied-at? (-> twixt-board? twixt-position? boolean?)]

  [twixt-board-put-peg
   (->i ([board twixt-board?]) #:rest [pegs (listof twixt-peg?)]

        #:pre/name (board pegs) "board must be unoccupied"
        (for/and ([peg (in-list pegs)])
          (twixt-board-unoccupied-at? board (twixt-peg-position peg)))

        #:pre/name (pegs) "peg positions must be unique"
        (list-elements-unique? (map twixt-peg-position pegs))

        [_ twixt-board?])]

  [twixt-board-pegs (-> twixt-board? (set/c twixt-peg?))]))

(require (for-syntax racket/base
                     syntax/parse)
         racket/bool
         racket/function
         racket/match
         racket/math
         racket/set
         rebellion/base/option
         rebellion/collection/entry
         rebellion/collection/hash
         rebellion/collection/immutable-vector
         rebellion/collection/list
         rebellion/collection/set
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         rebellion/type/enum
         rebellion/type/record)

(module+ test
  (require (submod "..")
           racket/contract/combinator
           rackunit))

;@------------------------------------------------------------------------------
;; Twixt data model

(define standard-twixt-board-size 24)
(define standard-twixt-board-cell-count (sqr standard-twixt-board-size))
(define standard-twixt-border-length (- standard-twixt-board-size 2))

(define twixt-index/c (integer-in 0 (sub1 standard-twixt-board-size)))

(define-enum-type twixt-player (red black))
(define-record-type twixt-position (row column) #:omit-root-binding)
(define-record-type twixt-peg (owner position) #:omit-root-binding)
(define-record-type twixt-board (grid-cells links))

(define-module-boundary-contract contracted:twixt-position
  constructor:twixt-position
  (-> #:row twixt-index/c #:column twixt-index/c twixt-position?)
  #:name-for-blame twixt-position)

(define-match-expander twixt-position
  (syntax-parser [(_ arg ...) #'(pattern:twixt-position arg ...)])
  (make-rename-transformer #'contracted:twixt-position))

(define-module-boundary-contract contracted:twixt-peg
  constructor:twixt-peg
  (-> #:owner twixt-player? #:position twixt-position? twixt-peg?)
  #:name-for-blame twixt-peg)

(define-match-expander twixt-peg
  (syntax-parser [(_ arg ...) #'(pattern:twixt-peg arg ...)])
  (make-rename-transformer #'contracted:twixt-peg))

(define (red-twixt-peg #:row row #:column column)
  (twixt-peg #:owner red #:position (twixt-position #:row row #:column column)))

(define (black-twixt-peg #:row row #:column column)
  (twixt-peg #:owner black
             #:position (twixt-position #:row row #:column column)))

(define empty-twixt-board
  (twixt-board
   #:grid-cells (make-immutable-vector standard-twixt-board-cell-count #f)
   #:links empty-hash))

(define (twixt-cell-index->position index)
  (define-values (row column)
    (quotient/remainder index standard-twixt-board-size))
  (twixt-position #:row row #:column column))

(define (twixt-position->cell-index position)
  (match-define (twixt-position #:row row #:column column) position)
  (+ (* row standard-twixt-board-size) column))

(module+ test
  (test-case "twixt cell indices should be in bijection with twixt positions"
    (for ([i (in-range standard-twixt-board-cell-count)])
      (define position (twixt-cell-index->position i))
      (check-equal? (twixt-position->cell-index position) i))))

(define (twixt-board-get-peg board position)
  (define cells (twixt-board-grid-cells board))
  (define owner (vector-ref cells (twixt-position->cell-index position)))
  (if owner
      (present (twixt-peg #:owner owner #:position position))
      absent))

(define (twixt-board-occupied-at? board position)
  (present? (twixt-board-get-peg board position)))

(define (twixt-board-unoccupied-at? board position)
  (absent? (twixt-board-get-peg board position)))

(define (twixt-board-put-peg board . pegs)
  (define new-cells (vector-copy-of (twixt-board-grid-cells board)))
  (for ([peg (in-list pegs)])
    (match-define (twixt-peg #:owner owner #:position position) peg)
    (vector-set! new-cells (twixt-position->cell-index position) owner))
  (twixt-board #:grid-cells new-cells #:links (twixt-board-links board)))

(define (twixt-board-pegs board)
  (transduce (twixt-board-grid-cells board)
             enumerating
             (bisecting enumerated-position enumerated-element)
             (filtering-values (negate false?))
             (mapping-keys twixt-cell-index->position)
             (mapping
              (λ (e)
                (match-define (entry position owner) e)
                (twixt-peg #:owner owner #:position position)))
             #:into into-set))

(module+ test
  (test-case "twixt-position"
    (check-not-exn (λ () (twixt-position #:row 0 #:column 0)))
    (check-not-exn (λ () (twixt-position #:row 23 #:column 23)))
    (check-exn exn:fail:contract:blame?
               (λ () (twixt-position #:row -1 #:column 0)))
    (check-exn exn:fail:contract:blame?
               (λ () (twixt-position #:row 0 #:column -1)))
    (check-exn exn:fail:contract:blame?
               (λ () (twixt-position #:row 24 #:column 0)))
    (check-exn exn:fail:contract:blame?
               (λ () (twixt-position #:row 0 #:column 24))))
  
  (test-case "twixt-board-occupied-at?"
    (define position (twixt-position #:row 3 #:column 18))
    (check-false (twixt-board-occupied-at? empty-twixt-board position)))

  (test-case "twixt-board-unoccupied-at?"
    (define position (twixt-position #:row 3 #:column 18))
    (check-true (twixt-board-unoccupied-at? empty-twixt-board position)))

  (test-case "twixt-board-get-peg"
    (define position (twixt-position #:row 10 #:column 10))

    (test-case "unoccupied"
      (check-equal? (twixt-board-get-peg empty-twixt-board position) absent))

    (test-case "occupied"
      (define peg (twixt-peg #:owner black #:position position))
      (define board (twixt-board-put-peg empty-twixt-board peg))
      (check-equal? (twixt-board-get-peg board position) (present peg))))
    
  (test-case "twixt-board-put-peg"

    (test-case "should fail when occupied"
      (define peg (red-twixt-peg #:row 12 #:column 9))
      (define board (twixt-board-put-peg empty-twixt-board peg))
      (define (put-again) (twixt-board-put-peg board peg))
      (check-exn exn:fail:contract:blame? put-again)
      (check-exn #rx"twixt-board-put-peg" put-again)
      (check-exn #rx"board must be unoccupied" put-again)
      (check-exn #rx"12" put-again)
      (check-exn #rx"9" put-again)
      (check-exn #rx"red" put-again))

    (test-case "should fail when given multiple pegs for same position"
      (define (put-both)
        (twixt-board-put-peg empty-twixt-board
                             (red-twixt-peg #:row 7 #:column 21)
                             (black-twixt-peg #:row 7 #:column 21)))
      (check-exn exn:fail:contract:blame? put-both)
      (check-exn #rx"twixt-board-put-peg" put-both)
      (check-exn #rx"peg positions must be unique" put-both)
      (check-exn #rx"7" put-both)
      (check-exn #rx"21" put-both)
      (check-exn #rx"red" put-both)
      (check-exn #rx"black" put-both))))

;@------------------------------------------------------------------------------
;; Utils

(define (list-elements-unique? lst)
  (equal? (set-count (list->set lst)) (list-size lst)))

(define (vector-copy-of vec)
  (define copy (make-vector (vector-length vec)))
  (vector-copy! copy 0 vec)
  copy)
