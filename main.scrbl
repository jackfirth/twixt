#lang scribble/manual

@(require (for-label pict
                     racket/base
                     racket/contract/base
                     racket/math
                     racket/sequence
                     racket/set
                     rebellion/base/option
                     twixt)
          scribble/decode
          scribble/example)

@(define make-evaluator
   (make-eval-factory
    (list 'racket/base
          'racket/contract/base
          'twixt)))

@(define (racket-reference-tech . text)
   (apply tech #:doc '(lib "scribblings/reference/reference.scrbl") text))

@(define can-be-match-expander
   (decode-content
    @list{Can also be used as a @racket-reference-tech{match expander}.}))

@title{TwixT}
@defmodule[twixt]

TwixT is an abstract, turn-based, two-player board game in which players try to
connect their two sides of the board while preventing the opponent from doing
the same. The @racketmodname[twixt] library provides a basic data model for
representing, displaying, and playing TwixT games.

@section{TwixT Boards}

A @deftech{TwixT board} is a 24-by-24 rectangular board on which games of TwixT
are played. Each space of the board contains a hole, in which a @tech{TwixT peg}
may be placed. The four corner spaces of the board do not contain holes; pegs
cannot be placed in corners. The non-corner spaces on the edge of the board make
up the @deftech{TwixT border rows}, and each player controls two rows on
opposite sides of the board.

@defproc[(twixt-board? [v any/c]) boolean?]{
 A predicate for @tech{TwixT boards}.}

@defthing[empty-twixt-board twixt-board?]{
 The empty @tech{TwixT board}, which contains no pegs or links.

 @(examples
   #:eval (make-evaluator) #:once
   (twixt-board-pict empty-twixt-board))}

@defproc[(sample-twixt-board) twixt-board?]{
 Creates a random @tech{TwixT board} with a few pegs and links on it. This
 function is mostly intended for REPL experimentation and for example boards in
 Scribble documentation.

 Currently, this function occasionally produces illegal boards. Hopefully that
 will be fixed someday.

 @(examples
   #:eval (make-evaluator) #:once
   (twixt-board-pict (sample-twixt-board)))}

@defproc[(twixt-board [peg twixt-peg?] ...) twixt-board?]{
 Constructs a @tech{TwixT board} with each @racket[peg] (and its associated
 links) inserted into the board. Pegs cannot overlap with each other, nor can
 their links. Any peg with a link must have a corresponding peg at the link's
 destination, although that peg need not include a link back to the source peg.

 @(examples
   #:eval (make-evaluator) #:once
   (twixt-board-pict
    (twixt-board
     (red-twixt-peg #:row 8 #:column 9 right-down-link)
     (red-twixt-peg #:row 9 #:column 11)
     (red-twixt-peg #:row 11 #:column 10 up-right-link)
     (black-twixt-peg #:row 15 #:column 12)
     (black-twixt-peg #:row 13 #:column 11 down-right-link))))}

@defproc[(twixt-board-get-peg [board twixt-board?] [position twixt-position?])
         (option/c twixt-peg?)]{
 Returns the @tech{TwixT peg} at @racket[position] of @racket[board], if there
 is one.

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define board (twixt-board (red-twixt-peg #:row 9 #:column 11))))
   (twixt-board-get-peg board (twixt-position #:row 9 #:column 11))
   (twixt-board-get-peg board (twixt-position #:row 4 #:column 18)))}

@defproc[(twixt-board-put-all-pegs [board twixt-board?]
                                   [pegs (sequence/c twixt-peg?)])
         twixt-board?]{
 Puts each @tech{TwixT peg} in @racket[pegs] onto @racket[board] at the peg's
 position. Two pegs on a board cannot occupy the same position, and links must
 be connected to pegs on both ends. If either of these conditions is violated,
 a contract failure is raised. Links between pegs cannot overlap, but
 enforcement of this property is not yet implemented.

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define board (twixt-board (red-twixt-peg #:row 9 #:column 11))))
   (twixt-board-pict
    (twixt-board-put-all-pegs
     board
     (list (red-twixt-peg #:row 7 #:column 12 down-left-link)
           (red-twixt-peg #:row 10 #:column 13 left-up-link)
           (red-twixt-peg #:row 10 #:column 9 right-up-link)
           (red-twixt-peg #:row 12 #:column 14 up-left-link)))))}

@defproc[(twixt-board-pegs [board twixt-board?]) (set/c twixt-peg?)]{
 Returns a @racket-reference-tech{set} of all @tech{TwixT pegs} placed on
 @racket[board].

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define board (sample-twixt-board)))
   (twixt-board-pegs board)
   (twixt-board-pict board))}

@defproc[(twixt-board-occupied-positions [board twixt-board?])
         (set/c twixt-position?)]{
 Returns a @racket-reference-tech{set} of all positions on @racket[board] that
 are occupied by @tech{TwixT pegs}.

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define board (sample-twixt-board)))
   (twixt-board-occupied-positions board)
   (twixt-board-pict board))}

@defproc[(twixt-board-links [board twixt-board?]) (set/c twixt-link?)]{
 Returns a @racket-reference-tech{set} of all @tech{TwixT links} between pegs on
 @racket[board].

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define board (sample-twixt-board)))
   (twixt-board-links board)
   (twixt-board-pict board))}

@subsection{TwixT Board Positions}

A @deftech{TwixT position} is a space on a @tech{TwixT board}. Each board has 24
rows and columns, so positions are represented by a pair of integers between 0
and 23, with the top-left corner of the board corresponding to row zero and
column zero.

@defproc[(twixt-position? [v any/c]) boolean?]{
 A predicate for @tech{TwixT positions}.}

@defproc[(twixt-position [#:row row twixt-index/c]
                         [#:column column twixt-index/c])
         twixt-position?]{
 Constructs a @tech{TwixT position} for the space that is @racket[row] spaces
 below and @racket[column] spaces to the right of the top left corner of the
 board. @can-be-match-expander

 @(examples
   #:eval (make-evaluator) #:once
   (twixt-board-pict (twixt-board (black-twixt-peg #:row 4 #:column 0)))
   (twixt-board-pict (twixt-board (red-twixt-peg #:row 1 #:column 3))))}

@defproc[(twixt-position-row [position twixt-position?]) twixt-index/c]{
 Returns the row of @racket[position].}

@defproc[(twixt-position-column [position twixt-position?]) twixt-index/c]{
 Returns the column of @racket[position].}

@defthing[standard-twixt-board-size natural? #:value 24]{
 A constant for the length of each side of a @tech{TwixT board}. Boards are
 always square shaped.}

@defthing[twixt-index/c flat-contract? #:value (integer-in 0 23)]{
 A @racket-reference-tech{flat contract} for integers that are within the bounds
 of a TwixT board, and can be used to reference positions on the board.}

@section{TwixT Players}

There are two players in a game of TwixT, referred to as @racket[red] and
@racket[black]. The @racket[red] player always moves first. Note that the actual
color of the displayed pieces may be different due to the use of a non-standard
stylesheet such as @racket[monochrome-twixt-stylesheet]. Regardless of the
actual piece color, the player who moves first is called the @racket[red]
player.

@defproc[(twixt-player? [v any/c]) boolean?]{
 A predicate for TwixT players.}

@deftogether[[
 @defthing[red twixt-player?]
 @defthing[black twixt-player?]]]{
 Constants for the red and black TwixT players. The @racket[red] player always
 moves first.}

@section{TwixT Pegs}

A @deftech{TwixT peg} is a piece owned by one of the two TwixT players and
placed somewhere on a @tech{TwixT board}. Each peg may have @tech{TwixT links}
attaching it to the player's other pegs.

@defproc[(twixt-peg? [v any/c]) boolean?]{
 A predicate for @tech{TwixT pegs}.}

@defproc[(twixt-peg
          [#:owner owner twixt-player?]
          [#:position position twixt-position?]
          [#:link-directions links (sequence/c twixt-link-direction?)])
         twixt-peg?]{
 Constructs a @tech{TwixT peg} at @racket[position] and owned by @racket[owner].
 Each link in @racket[links] specifies a direction in which the peg is linked to
 another peg. @can-be-match-expander

 This is a general-purpose constructor for twixt pegs. When the owning player
 and set of links are statically known, using @racket[red-twixt-peg] or @racket[
 black-twixt-peg] may be more readable.}

@defproc[(red-twixt-peg [#:row row twixt-index/c]
                        [#:column column twixt-index/c]
                        [link twixt-link-direction?] ...)
         twixt-peg?]{
 Constructs a @tech{TwixT peg} at @racket[row] and @racket[column], owned by the
 @racket[red] player. Each @racket[link] specifies a direction in which the peg
 is linked to another peg.

 @(examples
   #:eval (make-evaluator) #:once
   (twixt-board-pict
    (twixt-board
     (red-twixt-peg #:row 10 #:column 12
                    up-right-link
                    down-right-link
                    left-up-link)
     (red-twixt-peg #:row 8 #:column 13)
     (red-twixt-peg #:row 12 #:column 13)
     (red-twixt-peg #:row 9 #:column 10))))}

@defproc[(black-twixt-peg [#:row row twixt-index/c]
                          [#:column column twixt-index/c]
                          [link twixt-link-direction?] ...)
         twixt-peg?]{
 Constructs a @tech{TwixT peg} at @racket[row] and @racket[column], owned by the
 @racket[black] player. Each @racket[link] specifies a direction in which the
 peg is linked to another peg.

 @(examples
   #:eval (make-evaluator) #:once
   (twixt-board-pict
    (twixt-board
     (black-twixt-peg #:row 14 #:column 7 right-down-link)
     (black-twixt-peg #:row 15 #:column 9 right-up-link)
     (black-twixt-peg #:row 14 #:column 11 up-right-link)
     (black-twixt-peg #:row 12 #:column 12))))}

@defproc[(twixt-peg-owner [peg twixt-peg?]) twixt-player?]{
 Returns the player that owns @racket[peg].}

@defproc[(twixt-peg-position [peg twixt-peg?]) twixt-position?]{
 Returns the position where @racket[peg] is placed on the board.}

@defproc[(twixt-peg-links [peg twixt-peg?]) (set/c twixt-link?)]{
 Returns the set of @tech{TwixT links} attached to this peg.}

@defproc[(twixt-peg-link-directions [peg twixt-peg?])
         (set/c twixt-link-direction?)]{
 Returns the set of directions in which @racket[peg] is linked to other pegs.}

@subsection{TwixT Peg Links}

A @deftech{TwixT link} is a link between two @tech{TwixT pegs} owned by the same
player. Linked pegs must always be a knight's move apart, meaning two spaces
apart horizontally and one space apart vertically or vice-versa.

@defproc[(twixt-link? [v any/c]) boolean?]{
 A predicate for @tech{TwixT links}.}

@defproc[(twixt-link [#:owner owner twixt-player?]
                     [#:left-end left-end twixt-position?]
                     [#:right-end right-end twixt-position?])
         twixt-link?]{
 Constructs a @tech{TwixT link} between @racket[left-end] and @racket[right-end]
 and owned by @racket[owner]. The column of @racket[left-end] must be less than
 that of @racket[right-end]. @can-be-match-expander}

@defproc[(twixt-link-owner [link twixt-link?]) twixt-player?]{
 Returns the TwixT player that owns @racket[link].}

@defproc[(twixt-link-left-end [link twixt-link?]) twixt-position?]{
 Returns the position of the left end of @racket[link].}

@defproc[(twixt-link-right-end [link twixt-link?]) twixt-position?]{
 Returns the position of the right end of @racket[link].}

@defproc[(twixt-link-direction? [v any/c]) boolean?]{
 A predicate for the directions in which a peg may link to another peg.}

@deftogether[[
 @defthing[up-left-link twixt-link-direction?]
 @defthing[up-right-link twixt-link-direction?]
 @defthing[right-up-link twixt-link-direction?]
 @defthing[right-down-link twixt-link-direction?]
 @defthing[down-right-link twixt-link-direction?]
 @defthing[down-left-link twixt-link-direction?]
 @defthing[left-down-link twixt-link-direction?]
 @defthing[left-up-link twixt-link-direction?]]]{
 Constants for the eight directions in which one @tech{TwixT peg} may link to
 another. Each link travels two spaces in the first direction of it's name, then
 one space in the second direction.}

@section{Drawing TwixT Games}

@defproc[(twixt-board-pict
          [board twixt-board?]
          [#:stylesheet styles twixt-stylesheet? standard-twixt-stylesheet])
         pict?]{
 Draws @racket[board] using @racket[styles] to control the size and colors of
 the pieces.

 @(examples
   #:eval (make-evaluator) #:once
   (twixt-board-pict (sample-twixt-board)))}

@defproc[(twixt-stylesheet? [v any/c]) boolean?]{
 A predicate for TwixT stylesheets, which control various aspects of how TwixT
 boards are drawn. The two built-in stylesheets are @racket[
 standard-twixt-stylesheet] and @racket[monochrome-twixt-stylesheet].}

@defthing[standard-twixt-stylesheet twixt-stylesheet?]{
 A stylesheet that draws TwixT boards in the clasic red-and-black style. This is
 the default stylesheet.}

@defthing[monochrome-twixt-stylesheet twixt-stylesheet?]{
 A stylesheet that draws TwixT boards in black and white. White goes first.

 @(examples
   #:eval (make-evaluator) #:once
   (twixt-board-pict (sample-twixt-board)
                     #:stylesheet monochrome-twixt-stylesheet))}
