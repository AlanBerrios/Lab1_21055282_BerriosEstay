#lang racket

(define bit?
  (lambda (bitt)
       (if (and (integer? bitt)
            (or (= bitt 0)(= bitt 1))) true false)))

(provide (all-defined-out))