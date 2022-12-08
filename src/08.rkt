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
       (mmap char->number)))

(define (get-tree r c)
  (list-ref (list-ref input r) c))

(define width (length (first input)))
(define height (length input))

(define (part1)
  (define s1
    (for/fold ([talls (set)])
              ([r (range height)])
      (for/fold ([talls talls]
                 [tallest (get-tree r 0)]
                 #:result talls)
                ([c (range width)])
        (define t (get-tree r c))
        (if (< tallest t)
            (values (set-add talls (cons r c)) t)
            (values talls tallest)))))
  (define s2
    (for/fold ([talls s1])
              ([r (range height)])
      (for/fold ([talls talls]
                 [tallest (get-tree r (sub1 width))]
                 #:result talls)
                ([c (reverse (range width))])
        (define t (get-tree r c))
        (if (< tallest t)
            (values (set-add talls (cons r c)) t)
            (values talls tallest)))))
  (define s3
    (for/fold ([talls s2])
              ([c (range width)])
      (for/fold ([talls talls]
                 [tallest (get-tree 0 c)]
                 #:result talls)
                ([r (range height)])
        (define t (get-tree r c))
        (if (< tallest t)
            (values (set-add talls (cons r c)) t)
            (values talls tallest)))))
  (define s4
    (for/fold ([talls s3])
              ([c (range width)])
      (for/fold ([talls talls]
                 [tallest (get-tree (sub1 height) c)]
                 #:result talls)
                ([r (reverse (range height))])
        (define t (get-tree r c))
        (if (< tallest t)
            (values (set-add talls (cons r c)) t)
            (values talls tallest)))))
  (define s5
    (for/fold ([talls s4])
              ([r (range height)])
      (~> talls
          (set-add (cons r 0))
          (set-add (cons r (sub1 width))))))
  (define s6
    (for/fold ([talls s5])
              ([c (range width)])
      (~> talls
          (set-add (cons 0 c))
          (set-add (cons (sub1 height) c)))))
  (set-count s6))

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