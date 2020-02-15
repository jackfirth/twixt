#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [twixt-board? predicate/c]
  [empty-twixt-board twixt-board?]
  [twixt-board-pict (->* (twixt-board?) (#:stylesheet twixt-stylesheet?) pict?)]
  [twixt-stylesheet? predicate/c]
  [standard-twixt-stylesheet twixt-stylesheet?]
  [monochrome-twixt-stylesheet twixt-stylesheet?]))

(require pict
         racket/bool
         racket/function
         racket/list
         racket/match
         racket/math
         racket/sequence
         rebellion/base/option
         rebellion/collection/entry
         rebellion/collection/immutable-vector
         rebellion/collection/hash
         rebellion/collection/set
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         rebellion/type/enum
         rebellion/type/record
         rebellion/type/tuple)

(module+ test
  (require (submod "..")
           rackunit))

;@------------------------------------------------------------------------------
;; Pict stuff

(define-tuple-type point (x y))

(define (point- p q)
  (match-define (point x1 y1) p)
  (match-define (point x2 y2) q)
  (point (- x1 x2) (- y1 y2)))

(define-enum-type orientation (top bottom left right))

(define-tuple-type relative-position (x-ratio y-ratio))

(define center (relative-position 1/2 1/2))
(define top-left (relative-position 0 0))
(define top-right (relative-position 1 0))
(define bottom-left (relative-position 0 1))
(define bottom-right (relative-position 1 1))
(define center-left (relative-position 0 1/2))
(define center-right (relative-position 1 1/2))
(define top-center (relative-position 1/2 0))
(define bottom-center (relative-position 1/2 1))

(define (pict-relative-point pict position)
  (match-define (relative-position xr yr) position)
  (point (* xr (pict-width pict))
         (* yr (pict-height pict))))

(define-record-type pinned-pict (content pinned-position base-position))

(define (pict-pin base pinned
                  #:base-position base-position
                  #:pinned-position pinned-position)
  (define pinned-point
    (if (relative-position? pinned-position)
        (pict-relative-point pinned pinned-position)
        pinned-position))
  (define base-point
    (if (relative-position? base-position)
        (pict-relative-point base base-position)
        base-position))
  (match-define (point dx dy) (point- base-point pinned-point))
  (panorama (pin-over base dx dy pinned)))

(define (pict-pin-pinned base pinned)
  (match-define
    (pinned-pict #:content content
                 #:base-position base-position
                 #:pinned-position pinned-position)
    pinned)
  (pict-pin base content
            #:base-position base-position
            #:pinned-position pinned-position))

(define (pin-into-pict base) (make-fold-reducer pict-pin-pinned base))

(define (pict-tile-horizontally pict repetitions)
  (apply hc-append (make-list repetitions pict)))

(define (pict-tile-vertically pict repetitions)
  (apply vc-append (make-list repetitions pict)))

(define (pict-tile-rectangularly pict
                                 horizontal-repetitions
                                 vertical-repetitions)
  (define row (pict-tile-horizontally pict horizontal-repetitions))
  (pict-tile-vertically row vertical-repetitions))

(define (pict-add-inset-border pict edge
                               #:thickness thickness #:color [color #f])
  (define width
    (match edge
      [(or (== top) (== bottom)) (pict-width pict)]
      [(or (== left) (== right)) thickness]))
  (define height
    (match edge
      [(or (== top) (== bottom)) thickness]
      [(or (== left) (== right)) (pict-height pict)]))
  (define border
    (filled-rectangle width height #:color color #:draw-border? #f))
  (define position
    (match edge
      [(== top) top-center]
      [(== bottom) bottom-center]
      [(== left) center-left]
      [(== right) center-right]))
  (pict-pin pict border #:base-position position #:pinned-position position))

(define (pict-grid . rows)
  (define (row->pict row) (apply hc-append (sequence->list row)))
  (apply vc-append (map row->pict (sequence->list rows))))

;@------------------------------------------------------------------------------
;; Twixt data model

(define standard-twixt-board-size 24)
(define standard-twixt-board-cell-count (sqr standard-twixt-board-size))
(define standard-twixt-border-length (- standard-twixt-board-size 2))

(define-enum-type twixt-player (red black))
(define-record-type twixt-position (row column))
(define-record-type twixt-peg (owner position))
(define-record-type twixt-board (grid-cells links))

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
  (and owner
       (present (twixt-peg #:owner owner #:position position))
       absent))

(define (twixt-board-occupied-at? board position)
  (present? (twixt-board-get-peg board position)))

(define (twixt-board-unoccupied-at? board position)
  (absent? (twixt-board-get-peg board position)))

(define (vector-copy-of vec)
  (define copy (make-vector (vector-length vec)))
  (vector-copy! copy 0 vec)
  copy)

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

;@------------------------------------------------------------------------------
;; Twixt graphics

(define-record-type twixt-stylesheet
  (cell-size
   cell-color
   hole-diameter
   hole-color
   border-thickness
   peg-diameter
   line-thickness
   vertical-player-color
   horizontal-player-color
   vertical-player))

(define (twixt-stylesheet-player-color styles player)
  (if (equal? (twixt-stylesheet-vertical-player styles) player)
      (twixt-stylesheet-vertical-player-color styles)
      (twixt-stylesheet-horizontal-player-color styles)))

(define standard-twixt-stylesheet
  (twixt-stylesheet
   #:cell-size 24
   #:cell-color "Bisque"
   #:hole-diameter 4
   #:hole-color "Burlywood"
   #:border-thickness 4
   #:peg-diameter 16
   #:line-thickness 4
   #:vertical-player red
   #:vertical-player-color "Red"
   #:horizontal-player-color "Black"))

(define monochrome-twixt-stylesheet
  (twixt-stylesheet
   #:cell-size 24
   #:cell-color "Light Gray"
   #:hole-diameter 4
   #:hole-color "Dark Gray"
   #:border-thickness 4
   #:peg-diameter 16
   #:line-thickness 4
   #:vertical-player red
   #:vertical-player-color "White"
   #:horizontal-player-color "Black"))

(define (twixt-blank-cell-pict styles)
  (match-define (twixt-stylesheet #:cell-size size #:cell-color color) styles)
  (filled-rectangle size size #:draw-border? #f #:color color))

(define (twixt-cell-hole-pict styles)
  (match-define (twixt-stylesheet #:hole-diameter d #:hole-color color) styles)
  (disk d #:color color #:draw-border? #f))

(define (twixt-cell-pict styles)
  (define blank (twixt-blank-cell-pict styles))
  (define hole (twixt-cell-hole-pict styles))
  (pict-pin blank hole #:base-position center #:pinned-position center))

(define (twixt-border-row-pict styles #:with-border-on orientation)
  (match-define
    (twixt-stylesheet #:border-thickness thickness
                      #:vertical-player-color color)
    styles)
  (define cell (twixt-cell-pict styles))
  (define row (pict-tile-horizontally cell standard-twixt-border-length))
  (pict-add-inset-border row orientation #:thickness thickness #:color color))

(define (twixt-border-column-pict styles #:with-border-on orientation)
  (match-define
    (twixt-stylesheet #:border-thickness thickness
                      #:horizontal-player-color color)
    styles)
  (define cell (twixt-cell-pict styles))
  (define column (pict-tile-vertically cell standard-twixt-border-length))
  (pict-add-inset-border column orientation
                         #:thickness thickness #:color color))

(define (twixt-center-pict styles)
  (pict-tile-rectangularly (twixt-cell-pict styles)
                           standard-twixt-border-length
                           standard-twixt-border-length))

(define (empty-twixt-board-pict styles)
  (define corner (twixt-blank-cell-pict styles))
  (define top-row (twixt-border-row-pict styles #:with-border-on bottom))
  (define bottom-row (twixt-border-row-pict styles #:with-border-on top))
  (define left-column (twixt-border-column-pict styles #:with-border-on right))
  (define right-column (twixt-border-column-pict styles #:with-border-on left))
  (define center-cells (twixt-center-pict styles))
  (pict-grid
   (list corner top-row corner)
   (list left-column center-cells right-column)
   (list corner bottom-row corner)))

(define (twixt-board-pict board #:stylesheet [styles standard-twixt-stylesheet])
  (transduce (twixt-board-pegs board)
             (mapping (λ (peg) (twixt-peg-pict peg #:stylesheet styles)))
             #:into (pin-into-pict (empty-twixt-board-pict styles))))

(define (twixt-peg-pict peg
                        #:stylesheet [styles standard-twixt-stylesheet])
  (match-define
    (twixt-peg #:owner owner
               #:position (twixt-position #:row row #:column column))
    peg)
  (define diameter (twixt-stylesheet-peg-diameter styles))
  (define color (twixt-stylesheet-player-color styles owner))
  (define width (twixt-stylesheet-line-thickness styles))
  (define peg-pict (circle diameter #:border-color color #:border-width width))
  (define relative-x (/ (+ column 1/2) 24))
  (define relative-y (/ (+ row 1/2) 24))
  (pinned-pict #:content peg-pict
               #:base-position (relative-position relative-x relative-y)
               #:pinned-position center))

(module+ main
  (twixt-board-pict
   (twixt-board-put-peg empty-twixt-board
                        (red-twixt-peg #:row 1 #:column 1)
                        (black-twixt-peg #:row 1 #:column 22)
                        (red-twixt-peg #:row 22 #:column 1)
                        (black-twixt-peg #:row 22 #:column 22))))
