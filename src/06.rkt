#lang racket

(require "../lib.rkt")

(define input (string->list (first (problem-input 6))))

(define (search n)
  (let loop ([buffer input]
             [index n])
    (if (= n (set-count (list->set (take buffer n))))
        index
        (loop (rest buffer) (add1 index)))))

(define part1 (search 4))
  
(define part2 (search 14))

(show-solution part1 part2)