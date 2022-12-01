#lang racket

(require "../lib.rkt")

(define input (mmap string->number (problem-input-grouped-lines 1)))

(define calories (sort (map sum input) >))

(define part1 (first calories))

(define part2 (sum (take calories 3)))

(show-solution part1 part2)