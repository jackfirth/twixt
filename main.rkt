#lang racket/base

(require racket/contract/base)

(provide
 (all-from-out twixt/base)
 (contract-out
  [twixt-board (-> twixt-peg? ... twixt-board?)]
  [twixt-board-pict (->* (twixt-board?) (#:stylesheet twixt-stylesheet?) pict?)]
  [sample-twixt-board (-> twixt-board?)]
  [twixt-stylesheet? predicate/c]
  [standard-twixt-stylesheet twixt-stylesheet?]
  [monochrome-twixt-stylesheet twixt-stylesheet?]))

(require pict
         racket/bool
         racket/function
         racket/list
         racket/match
         racket/math
         racket/random
         racket/sequence
         racket/set
         rebellion/base/option
         rebellion/collection/entry
         rebellion/collection/immutable-vector
         rebellion/collection/hash
         rebellion/collection/set
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         rebellion/type/enum
         rebellion/type/record
         rebellion/type/tuple
         twixt/base)

(module+ test
  (require (submod "..")
           rackunit))

;@------------------------------------------------------------------------------
;; Data model stuff that shouldn't be allowed bypass twixt/base contracts

(define (twixt-board . pegs)
  (apply twixt-board-put-peg empty-twixt-board pegs))

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

(define-record-type pinned-line (source destination color thickness))

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

(define (pict-pin-line pict line)
  (match-define
    (pinned-line #:source relative-source
                 #:destination relative-destination
                 #:color color
                 #:thickness thickness)
    line)
  (define source
    (if (relative-position? relative-source)
        (pict-relative-point pict relative-source)
        relative-source))
  (define destination
    (if (relative-position? relative-destination)
        (pict-relative-point pict relative-destination)
        relative-destination))
  (match-define (point dx dy) (point- destination source))
  (define line-pict (linewidth thickness (colorize (pip-line dx dy 0) color)))
  (pin-over pict (point-x source) (point-y source) line-pict))

(define (pict-pin-pinned base pinned)
  (match pinned
    [(pinned-pict #:content content
                  #:base-position base-position
                  #:pinned-position pinned-position)
     (pict-pin base content
               #:base-position base-position
               #:pinned-position pinned-position)]
    [(? pinned-line?)
     (pict-pin-line base pinned)]))

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
   #:cell-size 20
   #:cell-color "Bisque"
   #:hole-diameter 3
   #:hole-color "Burlywood"
   #:border-thickness 3
   #:peg-diameter 12
   #:line-thickness 3
   #:vertical-player red
   #:vertical-player-color "Red"
   #:horizontal-player-color "Black"))

(define monochrome-twixt-stylesheet
  (twixt-stylesheet
   #:cell-size (twixt-stylesheet-cell-size standard-twixt-stylesheet)
   #:cell-color "Light Gray"
   #:hole-diameter (twixt-stylesheet-hole-diameter standard-twixt-stylesheet)
   #:hole-color "Dark Gray"
   #:border-thickness
   (twixt-stylesheet-border-thickness standard-twixt-stylesheet)
   #:peg-diameter (twixt-stylesheet-peg-diameter standard-twixt-stylesheet)
   #:line-thickness (twixt-stylesheet-line-thickness standard-twixt-stylesheet)
   #:vertical-player
   (twixt-stylesheet-vertical-player standard-twixt-stylesheet)
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
  (define board-with-pegs-pict
    (transduce (twixt-board-pegs board)
               (mapping (λ (peg) (twixt-peg-pict peg #:stylesheet styles)))
               #:into (pin-into-pict (empty-twixt-board-pict styles))))
  (transduce (twixt-board-links board)
             (mapping (λ (link) (twixt-link-pict link #:stylesheet styles)))
             #:into (pin-into-pict board-with-pegs-pict)))

(define (twixt-board-relative-position position)
  (match-define (twixt-position #:row row #:column column) position)
  (relative-position (/ (+ column 1/2) standard-twixt-board-size)
                     (/ (+ row 1/2) standard-twixt-board-size)))

(define (twixt-peg-pict peg #:stylesheet [styles standard-twixt-stylesheet])
  (match-define
    (twixt-peg #:owner owner
               #:position position)
    peg)
  (define diameter (twixt-stylesheet-peg-diameter styles))
  (define color (twixt-stylesheet-player-color styles owner))
  (define width (twixt-stylesheet-line-thickness styles))
  (define peg-pict (circle diameter #:border-color color #:border-width width))
  (pinned-pict #:content peg-pict
               #:base-position (twixt-board-relative-position position)
               #:pinned-position center))

(define (twixt-link-pict link #:stylesheet [styles standard-twixt-stylesheet])
  (match-define
    (placed-twixt-link
     #:owner owner
     #:left-end start
     #:right-end end)
    link)
  (pinned-line #:source (twixt-board-relative-position start)
               #:destination (twixt-board-relative-position end)
               #:color (twixt-stylesheet-player-color styles owner)
               #:thickness (twixt-stylesheet-line-thickness styles)))

(define (sample-twixt-board)
  (define red-pos1
    (twixt-position #:row (+ (random 16) 4) #:column (+ (random 16) 4)))
  (define black-pos1
    (twixt-position #:row (+ (random 16) 4) #:column (+ (random 16) 4)))
  (define red-link (random-ref all-twixt-links))
  (define black-link (random-ref all-twixt-links))
  (define red-pos2 (twixt-link-destination red-link red-pos1))
  (define black-pos2 (twixt-link-destination black-link black-pos1))
  (cond
    [(or (equal? red-pos1 black-pos1)
         (equal? red-pos1 black-pos2)
         (equal? red-pos2 black-pos1)
         (equal? red-pos2 black-pos2))
     (sample-twixt-board)]
    [else
     (twixt-board
      (twixt-peg #:owner red #:position red-pos1 #:links (set red-link))
      (twixt-peg #:owner red #:position red-pos2)
      (twixt-peg #:owner black #:position black-pos1 #:links (set black-link))
      (twixt-peg #:owner black #:position black-pos2))]))

(module+ main
  (twixt-board-pict
   (sample-twixt-board)))
