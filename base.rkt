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
  [twixt-link? predicate/c]
  [up-left-link twixt-link?]
  [up-right-link twixt-link?]
  [right-up-link twixt-link?]
  [right-down-link twixt-link?]
  [down-right-link twixt-link?]
  [down-left-link twixt-link?]
  [left-down-link twixt-link?]
  [left-up-link twixt-link?]
  [twixt-peg? predicate/c]
  [twixt-peg-owner (-> twixt-peg? twixt-player?)]
  [twixt-peg-position (-> twixt-peg? twixt-position?)]
  [twixt-peg-links (-> twixt-peg? (set/c twixt-link?))]
  [red-twixt-peg
   (-> #:row twixt-index/c #:column twixt-index/c twixt-link? ... twixt-peg?)]
  [black-twixt-peg
   (-> #:row twixt-index/c #:column twixt-index/c twixt-link? ... twixt-peg?)]
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
         racket/sequence
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
;; Twixt links and positions

(define standard-twixt-board-size 24)
(define standard-twixt-cell-count (sqr standard-twixt-board-size))
(define standard-twixt-border-length (- standard-twixt-board-size 2))

(define twixt-index/c (integer-in 0 (sub1 standard-twixt-board-size)))

(define-enum-type twixt-link
  (up-left-link
   up-right-link
   right-up-link
   right-down-link
   down-right-link
   down-left-link
   left-down-link
   left-up-link))

(define-enum-type twixt-player (red black))
(define-record-type twixt-position (row column) #:omit-root-binding)

(define-module-boundary-contract contracted:twixt-position
  constructor:twixt-position
  (-> #:row twixt-index/c #:column twixt-index/c twixt-position?)
  #:name-for-blame twixt-position)

(define-match-expander twixt-position
  (syntax-parser [(_ arg ...) #'(pattern:twixt-position arg ...)])
  (make-rename-transformer #'contracted:twixt-position))

(define (twixt-link-inverse link)
  (match link
    [(== up-left-link) down-right-link]
    [(== up-right-link) down-left-link]
    [(== right-up-link) left-down-link]
    [(== right-down-link) left-up-link]
    [(== down-right-link) up-left-link]
    [(== down-left-link) up-right-link]
    [(== left-down-link) right-up-link]
    [(== left-up-link) right-down-link]))

(define (twixt-link-destination link source-position)
  (match-define (twixt-position #:row r #:column c) source-position)
  (define r*
    (match link
      [(or (== up-left-link) (== up-right-link)) (- r 2)]
      [(or (== left-up-link) (== right-up-link)) (- r 1)]
      [(or (== left-down-link) (== right-down-link)) (+ r 1)]
      [(or (== down-left-link) (== down-right-link)) (+ r 2)]))
  (define c*
    (match link
      [(or (== left-down-link) (== left-up-link)) (- c 2)]
      [(or (== up-left-link) (== down-left-link)) (- c 1)]
      [(or (== up-right-link) (== down-right-link)) (+ c 1)]
      [(or (== right-up-link) (== right-down-link)) (+ c 2)]))
  (twixt-position #:row r* #:column c*))

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

  (test-case "twixt-link-inverse"
    (check-equal? (twixt-link-inverse up-left-link) down-right-link)
    (check-equal? (twixt-link-inverse right-down-link) left-up-link))
  
  (test-case "twixt-link-destination"
    (define source (twixt-position #:row 5 #:column 10))
    (check-equal? (twixt-link-destination up-left-link source)
                  (twixt-position #:row 3 #:column 9))
    (check-equal? (twixt-link-destination up-right-link source)
                  (twixt-position #:row 3 #:column 11))
    (check-equal? (twixt-link-destination right-up-link source)
                  (twixt-position #:row 4 #:column 12))
    (check-equal? (twixt-link-destination right-down-link source)
                  (twixt-position #:row 6 #:column 12))
    (check-equal? (twixt-link-destination down-right-link source)
                  (twixt-position #:row 7 #:column 11))
    (check-equal? (twixt-link-destination down-left-link source)
                  (twixt-position #:row 7 #:column 9))
    (check-equal? (twixt-link-destination left-down-link source)
                  (twixt-position #:row 6 #:column 8))
    (check-equal? (twixt-link-destination left-up-link source)
                  (twixt-position #:row 4 #:column 8))))

;@------------------------------------------------------------------------------
;; Twixt pegs

(define-record-type twixt-peg (owner position links) #:omit-root-binding)

(define (smart-constructor:twixt-peg #:owner owner
                                     #:position position
                                     #:links [links empty-set])
  (constructor:twixt-peg #:owner owner
                         #:position position
                         #:links (transduce links #:into into-set)))

(define-module-boundary-contract contracted:twixt-peg
  smart-constructor:twixt-peg
  (->* (#:owner twixt-player? #:position twixt-position?)
       (#:links (sequence/c twixt-link?))
       twixt-peg?)
  #:name-for-blame twixt-peg)

(define-match-expander twixt-peg
  (syntax-parser [(_ arg ...) #'(pattern:twixt-peg arg ...)])
  (make-rename-transformer #'contracted:twixt-peg))

(define (red-twixt-peg #:row row #:column column . links)
  (define position (twixt-position #:row row #:column column))
  (twixt-peg #:owner red #:position position #:links links))

(define (black-twixt-peg #:row row #:column column . links)
  (define position (twixt-position #:row row #:column column))
  (twixt-peg #:owner black #:position position #:links links))


(define (twixt-cell-index->position index)
  (define-values (row column)
    (quotient/remainder index standard-twixt-board-size))
  (twixt-position #:row row #:column column))

(define (twixt-position->cell-index position)
  (match-define (twixt-position #:row row #:column column) position)
  (+ (* row standard-twixt-board-size) column))

(module+ test
  (test-case "twixt cell indices should be in bijection with twixt positions"
    (for ([i (in-range standard-twixt-cell-count)])
      (define position (twixt-cell-index->position i))
      (check-equal? (twixt-position->cell-index position) i))))

;@------------------------------------------------------------------------------
;; Twixt boards

(define-record-type twixt-board (cell-owners cell-links))

(define empty-twixt-board
  (twixt-board
   #:cell-owners (make-immutable-vector standard-twixt-cell-count #f)
   #:cell-links (make-immutable-vector standard-twixt-cell-count empty-set)))

(define (twixt-board-get-peg board position)
  (define cell-owners (twixt-board-cell-owners board))
  (define cell-links (twixt-board-cell-links board))
  (define index (twixt-position->cell-index position))
  (define owner (vector-ref cell-owners index))
  (define links (vector-ref cell-links index))
  (cond
    [(not owner) absent]
    [else
     (define peg (twixt-peg #:owner owner #:position position #:links links))
     (present peg)]))

(define (twixt-board-occupied-positions board)
  (transduce (twixt-board-cell-owners board)
             enumerating
             (filtering (λ (e) (not (false? (enumerated-element e)))))
             (mapping enumerated-position)
             (mapping twixt-cell-index->position)
             #:into into-set))

(define (twixt-board-occupied-at? board position)
  (present? (twixt-board-get-peg board position)))

(define (twixt-board-unoccupied-at? board position)
  (absent? (twixt-board-get-peg board position)))

(define (twixt-board-put-peg board . pegs)
  (define new-owners (vector-copy-of (twixt-board-cell-owners board)))
  (define new-links (vector-copy-of (twixt-board-cell-links board)))
  (for ([peg (in-list pegs)])
    (match-define (twixt-peg #:owner owner #:position position #:links links)
      peg)
    (define index (twixt-position->cell-index position))
    (vector-set! new-owners index owner)
    (vector-set! new-links index links)
    (for ([link (in-set links)])
      (define linked-index
        (twixt-position->cell-index (twixt-link-destination link position)))
      (define new-destination-links
        (set-add (vector-ref new-links linked-index) (twixt-link-inverse link)))
      (vector-set! new-links linked-index new-destination-links)))
  (twixt-board #:cell-owners (vector->immutable-vector new-owners)
               #:cell-links (vector->immutable-vector new-links)))

(define (twixt-board-pegs board)
  (transduce (twixt-board-occupied-positions board)
             (mapping (λ (p) (present-value (twixt-board-get-peg board p))))
             #:into into-set))

(module+ test
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
      (check-equal? (twixt-board-get-peg board position) (present peg)))

    (test-case "links"
      (define first-peg (red-twixt-peg #:row 10 #:column 10))
      (define second-peg (red-twixt-peg #:row 12 #:column 11 up-left-link))
      (define board
        (twixt-board-put-peg empty-twixt-board first-peg second-peg))
      (check-equal?
       (twixt-board-get-peg board (twixt-position #:row 10 #:column 10))
       (present (red-twixt-peg #:row 10 #:column 10 down-right-link)))
      (check-equal?
       (twixt-board-get-peg board (twixt-position #:row 12 #:column 11))
       (present (red-twixt-peg #:row 12 #:column 11 up-left-link)))))
    
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
