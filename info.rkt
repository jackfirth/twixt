#lang info

(define collection "twixt")

(define scribblings
  (list (list "main.scrbl"
              (list)
              (list 'library)
              "twixt")))

(define deps
  (list "pict-lib"
        "rebellion"
        "base"))

(define build-deps
  (list "pict-doc"
        "racket-doc"
        "rackunit-lib"
        "scribble-lib"))
