#lang racket

(define int>0?
  (lambda (xx)
       (if (and (integer? xx)
            (>= xx 0)) true false)))

(provide (all-defined-out))