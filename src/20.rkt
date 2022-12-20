#lang curly-fn racket

(require "../lib.rkt")

(define input
  (map string->number (problem-input 20)))

(struct element (value [prev #:mutable] [next #:mutable]))

(define (parse)
  (let ([hd (element (first input) #f #f)])
    (for/fold ([curr hd]
               [elements `(,hd)]
               #:result (values (begin (set-element-next! curr hd)
                                       (set-element-prev! hd curr)
                                       (reverse elements))))
              ([i (rest input)])
      (define next (element i curr #f))
      (set-element-next! curr next)
      (values next (cons next elements)))))

(define (mix! elements key)
  (for ([e elements])
    (set-element-next! (element-prev e) (element-next e))
    (set-element-prev! (element-next e) (element-prev e))
    (let loop ([i (% (* (element-value e) key) (sub1 (length elements)))]
               [next (element-next e)])
      (cond
        [(zero? i)
         (set-element-next! (element-prev next) e)
         (set-element-prev! e (element-prev next))
         (set-element-next! e next)
         (set-element-prev! next e)]
        [(positive? i) (loop (sub1 i) (element-next next))]
        [(negative? i) (loop (add1 i) (element-prev next))]))))

(define (grove elements key)
  (define 0th
    (let loop ([curr (first elements)])
      (if (= (element-value curr) 0)
          curr
          (loop (element-next curr)))))
  (define (get e i)
    (for/fold ([curr e])
              ([_ (in-range i)])
      (element-next curr)))
  (* (+ (element-value (get 0th (% 1000 (sub1 (length elements)))))
        (element-value (get 0th (% 2000 (sub1 (length elements)))))
        (element-value (get 0th (% 3000 (sub1 (length elements))))))
     key))

(define part1
  (let ([elements (parse)])
    (mix! elements 1)
    (grove elements 1)))

(define part2
  (let ([elements (parse)])
    (for ([_ (in-range 10)])
      (mix! elements 811589153))
    (grove elements 811589153)))

(show-solution part1 part2)