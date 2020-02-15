#lang scribble/manual

@(require (for-label pict
                     racket/base
                     racket/contract/base
                     twixt)
          scribble/example)

@(define make-evaluator
   (make-eval-factory
    (list 'racket/base
          'racket/contract/base
          'twixt)))

@title{TwixT}
@defmodule[twixt]

@defproc[(twixt-board? [v any/c]) boolean?]{
 A predicate for TwixT boards.}

@defthing[empty-twixt-board twixt-board?]{
 The empty twixt board.}

@section{Drawing TwixT Games}

@defproc[(twixt-board-pict
          [board twixt-board?]
          [#:stylesheet styles twixt-stylesheet? standard-twixt-stylesheet])
         pict?]{
 Draws @racket[board] using @racket[styles] to control the size and colors of
 the pieces.

 @(examples
   #:eval (make-evaluator) #:once
   (twixt-board-pict empty-twixt-board))}

@defproc[(twixt-stylesheet? [v any/c]) boolean?]{
 A predicate for TwixT stylesheets, which control various aspects of how TwixT
 boards are drawn. The two built-in stylesheets are @racket[
 standard-twixt-stylesheet] and @racket[monochrome-twixt-stylesheet].}

@defthing[standard-twixt-stylesheet twixt-stylesheet?]{
 A stylesheet that draws TwixT boards in the clasic red-and-black style.}

@defthing[monochrome-twixt-stylesheet twixt-stylesheet?]{
 A stylesheet that draws TwixT boards in black and white.

 @(examples
   #:eval (make-evaluator) #:once
   (twixt-board-pict empty-twixt-board
                     #:stylesheet monochrome-twixt-stylesheet))}
