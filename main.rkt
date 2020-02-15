#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [twixt-board? predicate/c]
  [empty-twixt-board twixt-board?]
  [twixt-board-pict (->* (twixt-board?) (#:stylesheet twixt-stylesheet?) pict?)]
  [twixt-stylesheet? predicate/c]
  [standard-twixt-stylesheet twixt-stylesheet?]))

(require pict
         racket/list
         racket/match
         racket/math
         rebellion/collection/immutable-vector
         rebellion/collection/hash
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         rebellion/type/enum
         rebellion/type/record
         rebellion/type/tuple)

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
  (define (row->pict row) (apply hc-append row))
  (apply vc-append (map row->pict rows)))

;@------------------------------------------------------------------------------
;; Twixt data model

(define standard-twixt-board-side-length 24)
(define standard-twixt-board-size (sqr standard-twixt-board-side-length))
(define standard-twixt-border-length (- standard-twixt-board-side-length 2))

(define-enum-type twixt-player (red black))
(define-record-type twixt-board (grid-cells links))

(define empty-twixt-board
  (twixt-board
   #:grid-cells (make-immutable-vector standard-twixt-board-size #f)
   #:links empty-hash))

(define (twixt-cell-index->position index) #f)

;@------------------------------------------------------------------------------
;; Twixt graphics

(define-record-type twixt-stylesheet
  (cell-size
   cell-color
   hole-diameter
   hole-color
   border-thickness
   vertical-player-color
   horizontal-player-color))

(define standard-twixt-stylesheet
  (twixt-stylesheet
   #:cell-size 24
   #:cell-color "Bisque"
   #:hole-diameter 4
   #:hole-color "Burlywood"
   #:border-thickness 4
   #:vertical-player-color "Red"
   #:horizontal-player-color "Black"))

(define twixt-border-thickness 4)

(define (twixt-player-color player)
  (match player
    [(== red) "red"]
    [(== black) "black"]))

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

(define (twixt-peg-pict owner) #f)

(define (twixt-board-pict board #:stylesheet [styles standard-twixt-stylesheet])
  (match-define (twixt-board #:grid-cells cells #:links links) board)
  (define pict-with-pegs
    (transduce cells
               enumerating
               (filtering (λ (e) (twixt-player? (enumerated-element e))))
               (mapping enumerated-peg->pict)
               #:into (pin-into-pict (empty-twixt-board-pict styles))))
  (transduce links
             (mapping link-entry->pict)
             #:into (pin-into-pict pict-with-pegs)))

(define (enumerated-peg->pict peg)
  (define index (enumerated-position peg))
  (define owner (enumerated-element peg))
  (pinned-pict #:content (twixt-peg-pict owner)
               #:position (twixt-cell-index->position index)))

(define (link-entry->pict ent)
   #f)

(module+ main
  (empty-twixt-board-pict standard-twixt-stylesheet))
