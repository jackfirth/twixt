#lang info

(define collection "twixt")

(define scribblings
  (list (list "main.scrbl"
              (list 'multi-page)
              (list 'library)
              "twixt")))

(define deps
  (list "base"))

(define build-deps
  (list "racket-doc"
        "rackunit-lib"
        "scribble-lib"))
