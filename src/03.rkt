#lang racket

(require "../lib.rkt")

(define input (map string->list (problem-input 3)))

;; priority : char? -> natural?
(define (priority c)
  (define lower-offset (sub1 (char->integer #\a)))
  (define upper-offset (sub1 (char->integer #\A)))
  (if (char-lower-case? c)
      (- (char->integer c) lower-offset)
      (+ 26 (- (char->integer c) upper-offset))))

;; unique : (listof char?) -> char?
(define (unique rucksack)
  (define len (/ (length rucksack) 2))
  (first (set-intersect (take rucksack len) (drop rucksack len))))

;; unique : (list/c (listof char?) (listof char?) (listof char?)) -> char?
(define (badge rucksacks)
  (first (apply set-intersect rucksacks)))

(define part1
  (for/sum ([rucksack input])
    (priority (unique rucksack))))

(define part2
  (for/sum ([rucksacks (in-slice 3 input)])
    (priority (badge rucksacks))))

(show-solution part1 part2)