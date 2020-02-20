#lang racket/base

(require racket/contract/base)

(provide twixt-peg
         twixt-position
         placed-twixt-link)

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
  [twixt-link-inverse (-> twixt-link? twixt-link?)]
  [twixt-link-destination (-> twixt-link? twixt-position? twixt-position?)]
  [all-twixt-links (set/c twixt-link?)]
  [up-left-link twixt-link?]
  [up-right-link twixt-link?]
  [right-up-link twixt-link?]
  [right-down-link twixt-link?]
  [down-right-link twixt-link?]
  [down-left-link twixt-link?]
  [left-down-link twixt-link?]
  [left-up-link twixt-link?]
  [placed-twixt-link? predicate/c]
  [placed-twixt-link-owner (-> placed-twixt-link? twixt-player?)]
  [placed-twixt-link-left-end (-> placed-twixt-link? twixt-position?)]
  [placed-twixt-link-right-end (-> placed-twixt-link? twixt-position?)]
  [twixt-peg? predicate/c]
  [twixt-peg-owner (-> twixt-peg? twixt-player?)]
  [twixt-peg-position (-> twixt-peg? twixt-position?)]
  [twixt-peg-links (-> twixt-peg? (set/c twixt-link?))]
  [twixt-peg-placed-links (-> twixt-peg? (set/c placed-twixt-link?))]
  [red-twixt-peg
   (-> #:row twixt-index/c #:column twixt-index/c twixt-link? ... twixt-peg?)]
  [black-twixt-peg
   (-> #:row twixt-index/c #:column twixt-index/c twixt-link? ... twixt-peg?)]
  [twixt-board? predicate/c]
  [empty-twixt-board twixt-board?]
  [twixt-board-get-peg (-> twixt-board? twixt-position? (option/c twixt-peg?))]
  [twixt-board-occupied-at? (-> twixt-board? twixt-position? boolean?)]
  [twixt-board-unoccupied-at? (-> twixt-board? twixt-position? boolean?)]

  [twixt-board-put-all-pegs
   (->i ([board twixt-board?] [pegs (sequence/c twixt-peg?)])

        #:pre/name (board pegs) "board must be unoccupied"
        (for/and ([peg pegs])
          (twixt-board-unoccupied-at? board (twixt-peg-position peg)))

        #:pre/name (pegs) "peg positions must be unique"
        (peg-positions-unique? pegs)

        #:pre/name (board pegs) "links must have pegs at both ends"
        (block
         (define peg-positions
           (transduce pegs (mapping twixt-peg-position) #:into into-set))
         (define occupied
           (set-union (twixt-board-occupied-positions board) peg-positions))
         (for*/and ([peg pegs]
                    [link (in-set (twixt-peg-placed-links peg))])
           (and (set-member? occupied (placed-twixt-link-left-end link))
                (set-member? occupied (placed-twixt-link-right-end link)))))

        [_ twixt-board?])]

  [twixt-board-occupied-positions (-> twixt-board? (set/c twixt-position?))]
  [twixt-board-pegs (-> twixt-board? (set/c twixt-peg?))]
  [twixt-board-links (-> twixt-board? (set/c placed-twixt-link?))]))

(require (for-syntax racket/base
                     syntax/parse)
         racket/block
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
         rebellion/collection/multiset
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
;; Twixt positions

(define standard-twixt-board-size 24)
(define standard-twixt-cell-count (sqr standard-twixt-board-size))
(define standard-twixt-border-length (- standard-twixt-board-size 2))

(define twixt-index/c (integer-in 0 (sub1 standard-twixt-board-size)))

(define-record-type twixt-position (row column) #:omit-root-binding)

(define-module-boundary-contract contracted:twixt-position
  constructor:twixt-position
  (-> #:row twixt-index/c #:column twixt-index/c twixt-position?)
  #:name-for-blame twixt-position)

(define-match-expander twixt-position
  (syntax-parser [(_ arg ...) #'(pattern:twixt-position arg ...)])
  (make-rename-transformer #'contracted:twixt-position))

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
      (check-equal? (twixt-position->cell-index position) i)))

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
               (λ () (twixt-position #:row 0 #:column 24)))))

;@------------------------------------------------------------------------------
;; Twixt links

(define-enum-type twixt-player (red black))

(define-enum-type twixt-link
  (up-left-link
   up-right-link
   right-up-link
   right-down-link
   down-right-link
   down-left-link
   left-down-link
   left-up-link))

(define all-twixt-links
  (set up-left-link
       up-right-link
       right-up-link
       right-down-link
       down-right-link
       down-left-link
       left-down-link
       left-up-link))

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

(define-record-type placed-twixt-link (owner left-end right-end)
  #:omit-root-binding)

(define-module-boundary-contract contracted:placed-twixt-link
  constructor:placed-twixt-link
  (-> #:owner twixt-player?
      #:left-end twixt-position?
      #:right-end twixt-position?
      placed-twixt-link?)
  #:name-for-blame placed-twixt-link)

(define-match-expander placed-twixt-link
  (syntax-parser [(_ arg ...) #'(pattern:placed-twixt-link arg ...)])
  (make-rename-transformer #'contracted:placed-twixt-link))

(module+ test
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

(define (twixt-peg-placed-links peg)
  (match-define (twixt-peg #:owner owner #:position source #:links links) peg)
  (define (place link)
    (define destination (twixt-link-destination link source))
    (define pointed-downwards?
      (< (twixt-position-column source) (twixt-position-column destination)))
    (placed-twixt-link #:owner owner
                       #:left-end (if pointed-downwards? source destination)
                       #:right-end (if pointed-downwards? destination source)))
  (transduce links (mapping place) #:into into-set))

(module+ test
  (test-case "twixt-peg-placed-links"
    (check-equal?
     (twixt-peg-placed-links
      (red-twixt-peg #:row 10
                     #:column 10
                     up-left-link
                     right-up-link
                     right-down-link))
     (set
      (placed-twixt-link
       #:left-end (twixt-position #:column 10 #:row 10)
       #:owner red
       #:right-end (twixt-position #:column 12 #:row 11))
      (placed-twixt-link
       #:left-end (twixt-position #:column 9 #:row 8)
       #:owner red
       #:right-end (twixt-position #:column 10 #:row 10))
      (placed-twixt-link
       #:left-end (twixt-position #:column 10 #:row 10)
       #:owner red
       #:right-end (twixt-position #:column 12 #:row 9))))))

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

(define (twixt-board-put-all-pegs board pegs)
  (define new-owners (vector-copy-of (twixt-board-cell-owners board)))
  (define new-links (vector-copy-of (twixt-board-cell-links board)))
  (for ([peg pegs])
    (match-define (twixt-peg #:owner owner #:position position #:links links)
      peg)
    (define index (twixt-position->cell-index position))
    (vector-set! new-owners index owner)
    (define previous-links (vector-ref new-links index))
    (vector-set! new-links index (set-union previous-links links))
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

(define (twixt-board-links board)
  (transduce (twixt-board-pegs board)
             (append-mapping twixt-peg-placed-links)
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
      (define board (twixt-board-put-all-pegs empty-twixt-board (list peg)))
      (check-equal? (twixt-board-get-peg board position) (present peg)))

    (test-case "links"
      (define first-peg (red-twixt-peg #:row 10 #:column 10))
      (define second-peg (red-twixt-peg #:row 12 #:column 11 up-left-link))
      (define board
        (twixt-board-put-all-pegs empty-twixt-board
                                  (list first-peg second-peg)))
      (check-equal?
       (twixt-board-get-peg board (twixt-position #:row 10 #:column 10))
       (present (red-twixt-peg #:row 10 #:column 10 down-right-link)))
      (check-equal?
       (twixt-board-get-peg board (twixt-position #:row 12 #:column 11))
       (present (red-twixt-peg #:row 12 #:column 11 up-left-link)))))
    
  (test-case "twixt-board-put-peg"

    (test-case "should fail when occupied"
      (define peg (red-twixt-peg #:row 12 #:column 9))
      (define board (twixt-board-put-all-pegs empty-twixt-board (list peg)))
      (define (put-again) (twixt-board-put-all-pegs board (list peg)))
      (check-exn exn:fail:contract:blame? put-again)
      (check-exn #rx"twixt-board-put-all-pegs" put-again)
      (check-exn #rx"board must be unoccupied" put-again)
      (check-exn #rx"12" put-again)
      (check-exn #rx"9" put-again)
      (check-exn #rx"red" put-again))

    (test-case "should fail when given multiple pegs for same position"
      (define (put-both)
        (twixt-board-put-all-pegs empty-twixt-board
                                  (list (red-twixt-peg #:row 7 #:column 21)
                                        (black-twixt-peg #:row 7 #:column 21))))
      (check-exn exn:fail:contract:blame? put-both)
      (check-exn #rx"twixt-board-put-all-pegs" put-both)
      (check-exn #rx"peg positions must be unique" put-both)
      (check-exn #rx"7" put-both)
      (check-exn #rx"21" put-both)
      (check-exn #rx"red" put-both)
      (check-exn #rx"black" put-both))

    (test-case "should fail when given links without pegs"
      (define (put-mislinked)
        (define pegs (list (red-twixt-peg #:row 10 #:column 10 up-left-link)))
        (twixt-board-put-all-pegs empty-twixt-board pegs))
      (check-exn exn:fail:contract:blame? put-mislinked)
      (check-exn #rx"twixt-board-put-all-pegs" put-mislinked)
      (check-exn #rx"links must have pegs at both ends" put-mislinked)
      (check-exn #rx"up-left-link" put-mislinked))

    (test-case "should allow links between simultaneously-added pegs"
      (define ((make-put-call . pegs))
        (twixt-board-put-all-pegs empty-twixt-board pegs))
      (check-not-exn
       (make-put-call
        (red-twixt-peg #:row 10 #:column 10 up-left-link)
        (red-twixt-peg #:row 8 #:column 9)))
      (check-not-exn
       (make-put-call
        (red-twixt-peg #:row 10 #:column 10)
        (red-twixt-peg #:row 8 #:column 9 down-right-link)))
      (check-not-exn
       (make-put-call
        (red-twixt-peg #:row 10 #:column 10 up-left-link)
        (red-twixt-peg #:row 8 #:column 9 down-right-link)))))

  (test-case "twixt board peg and link collections"
    (define board
      (twixt-board-put-all-pegs
       empty-twixt-board
       (list (red-twixt-peg #:row 5 #:column 5)
             (black-twixt-peg #:row 10 #:column 10)
             (red-twixt-peg #:row 6 #:column 7 left-up-link)
             (black-twixt-peg #:row 12 #:column 9 up-right-link))))
    (define pegs (twixt-board-pegs board))
    (define links (twixt-board-links board))
    (check-equal? (set-count pegs) 4)
    (check-equal? (set-count links) 2)
    (check-true
     (set-member? pegs (red-twixt-peg #:row 5 #:column 5 right-down-link)))
    (check-true
     (set-member? pegs (black-twixt-peg #:row 10 #:column 10 down-left-link)))))

;@------------------------------------------------------------------------------
;; Utils

(define (peg-positions-unique? pegs)
  (define positions (for/multiset ([peg pegs]) (twixt-peg-position peg)))
  (equal? (multiset-size positions)
          (set-count (multiset-unique-elements positions))))

(define (vector-copy-of vec)
  (define copy (make-vector (vector-length vec)))
  (vector-copy! copy 0 vec)
  copy)
