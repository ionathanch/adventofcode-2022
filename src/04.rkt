#lang racket

(require "../lib.rkt")

(define input
  (for/list ([assn (problem-input 4)])
    (map string->number (regexp-match* #px"\\d+" assn))))

(define (contains? l1 r1 l2 r2)
  (or (and (<= l1 l2) (>= r1 r2))
      (and (<= l2 l1) (>= r2 r1))))

(define (overlaps? l1 r1 l2 r2)
  (not (or (> l2 r1) (> l1 r2))))

(define part1
  (count (∂ apply contains?) input))

(define part2
  (count (∂ apply overlaps?) input))

(show-solution part1 part2)