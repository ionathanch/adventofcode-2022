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
  (for ([r (range height)])
    (set-add! talls (cons r 0))
    (set-add! talls (cons r (sub1 width)))
    (for/fold ([tallest (get-tree r 0)])
              ([c (range width)])
      (define t (get-tree r c))
      (if (< tallest t)
          (begin (set-add! talls (cons r c)) t)
          tallest))
    (for/fold ([tallest (get-tree r (sub1 width))])
              ([c (reverse (range width))])
      (define t (get-tree r c))
      (if (< tallest t)
          (begin (set-add! talls (cons r c)) t)
          tallest)))
  (for ([c (range width)])
    (set-add! talls (cons 0 c))
    (set-add! talls (cons (sub1 height) c))
    (for/fold ([tallest (get-tree 0 c)])
              ([r (range height)])
      (define t (get-tree r c))
      (if (< tallest t)
          (begin (set-add! talls (cons r c)) t)
          tallest))
    (for/fold ([tallest (get-tree (sub1 height) c)])
              ([r (reverse (range height))])
      (define t (get-tree r c))
      (if (< tallest t)
          (begin (set-add! talls (cons r c)) t)
          tallest)))
  (set-count talls))

(define (score r c)
  (define t (get-tree r c))
  (define s1
    (let loop ([c* (add1 c)])
      (if (or (>= c* (sub1 width))
              (>= (get-tree r c*) t))
          (- c* c)
          (loop (add1 c*)))))
  (define s2
    (let loop ([c* (sub1 c)])
      (if (or (<= c* 0)
              (>= (get-tree r c*) t))
          (- c c*)
          (loop (sub1 c*)))))
  (define s3
    (let loop ([r* (add1 r)])
      (if (or (>= r* (sub1 height))
              (>= (get-tree r* c) t))
          (- r* r)
          (loop (add1 r*)))))
  (define s4
    (let loop ([r* (sub1 r)])
      (if (or (<= r* 0)
              (>= (get-tree r* c) t))
          (- r r*)
          (loop (sub1 r*)))))
  (* s1 s2 s3 s4))

(define part2
  (maximum
   (for*/list ([r (range 1 (sub1 height))]
               [c (range 1 (sub1 width))])
     (score r c))))

(show-solution (part1) part2)