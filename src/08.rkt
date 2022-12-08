#lang racket

(require "../lib.rkt")

(define test
  '("30373"
    "25512"
    "65332"
    "33549"
    "35390"))

(define input
  (~>> (problem-input 8) #;test
       (map string->list)
       (mmap char->number)
       (map list->vector)
       list->vector))

(define (get-tree r c)
  (vector-ref (vector-ref input r) c))

(define width (vector-length (vector-first input)))
(define height (vector-length input))

(define (part1)
  (define talls (mutable-set))
  (for ([r (in-range height)])
    (set-add! talls (cons r 0))
    (set-add! talls (cons r (sub1 width)))
    (for/fold ([tallest (get-tree r 0)])
              ([c (in-range width)])
      (define t (get-tree r c))
      (if (< tallest t)
          (begin (set-add! talls (cons r c)) t)
          tallest))
    (for/fold ([tallest (get-tree r (sub1 width))])
              ([c (in-reverse-range width)])
      (define t (get-tree r c))
      (if (< tallest t)
          (begin (set-add! talls (cons r c)) t)
          tallest)))
  (for ([c (in-range width)])
    (set-add! talls (cons 0 c))
    (set-add! talls (cons (sub1 height) c))
    (for/fold ([tallest (get-tree 0 c)])
              ([r (in-range height)])
      (define t (get-tree r c))
      (if (< tallest t)
          (begin (set-add! talls (cons r c)) t)
          tallest))
    (for/fold ([tallest (get-tree (sub1 height) c)])
              ([r (in-reverse-range height)])
      (define t (get-tree r c))
      (if (< tallest t)
          (begin (set-add! talls (cons r c)) t)
          tallest)))
  (set-count talls))

(define (score r c)
  (define t (get-tree r c))
  (* (for/last ([c* (in-range (add1 c) width)]
                #:final (>= (get-tree r c*) t))
       (- c* c))
     (for/last ([c* (in-reverse-range c)]
                #:final (>= (get-tree r c*) t))
       (- c c*))
     (for/last ([r* (in-range (add1 r) height)]
                #:final (>= (get-tree r* c) t))
       (- r* r))
     (for/last ([r* (in-reverse-range r)]
                #:final (>= (get-tree r* c) t))
       (- r r*))))

(define part2
  (maximum
   (for*/list ([r (in-range 1 (sub1 height))]
               [c (in-range 1 (sub1 width))])
     (score r c))))

(show-solution (part1) part2)