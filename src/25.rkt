#lang racket

(require "../lib.rkt")

(define (snafu->number s)
  (for/sum ([c (reverse (string->list s))]
            [n (in-naturals)])
    (* (expt 5 n)
       (match c
         [#\= -2]
         [#\- -1]
         [_ (char->number c)]))))

(define (number->snafu n)
  (let loop ([n n]
             [cs '()])
    (if (zero? n) (list->string cs)
        (match (% n 5)
          [4 (loop (/ (+ n 1) 5) (cons #\- cs))]
          [3 (loop (/ (+ n 2) 5) (cons #\= cs))]
          [2 (loop (/ (- n 2) 5) (cons #\2 cs))]
          [1 (loop (/ (- n 1) 5) (cons #\1 cs))]
          [0 (loop (/ n 5) (cons #\0 cs))]))))

(define part1
  (number->snafu
   (for/sum ([req (problem-input 25)])
     (snafu->number req))))

(show-solution part1 #f)