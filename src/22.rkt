#lang racket

(require "../lib.rkt"
         data/queue)

(match-define `(,board (,path))
  (problem-input-grouped-lines 22))

(define part1 #f)
(define part2 #f)

(show-solution part1 part2)