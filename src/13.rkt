#lang curly-fn racket

(require "../lib.rkt")

(define input
  (~>> (problem-input-grouped-lines 13)
       (mmap #{string-replace % "," " "})
       (mmap #{call-with-input-string % read})))

(define (compare l r)
  (match (list l r)
    [`(,(? number? l) ,(? number? r)) #:when (< l r) #t]
    [`(,(? number? l) ,(? number? r)) #:when (> l r) #f]
    [`(,(? number? n) ,(? number? n)) 'continue]
    [`(() ()) 'continue]
    [`(() ,_) #t]
    [`(,_ ()) #f]
    [`(,(? list?) ,(? number?)) (compare l `(,r))]
    [`(,(? number?) ,(? list?)) (compare `(,l) r)]
    [`((,l ,ls ...) (,r ,rs ...))
     (match (compare l r)
       ['continue (compare ls rs)]
       [b b])]))

(define part1
  (for/sum ([i input]
            [j (in-range 1 +inf.0)])
    (if (compare (first i) (second i)) j 0)))

(define part2
  (let ([packets (sort (apply append `(([[2]] [[6]]) . ,input)) compare)])
    (* (add1 (index-of packets '[[2]]))
       (add1 (index-of packets '[[6]])))))

(show-solution part1 part2)